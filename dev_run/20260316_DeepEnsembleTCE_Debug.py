import sys
sys.path.append('../dev')

import argparse
import torch
from torch.utils.data import DataLoader, TensorDataset
from datasets import load_dataset
from OutputHeads import DeterministicHead
from BaseModels import TCN
from DeepEnsemble import DeepEnsemble

parser = argparse.ArgumentParser()
parser.add_argument('--seq_len', type=int, default=64)
parser.add_argument('--horizon', type=int, default=1)
parser.add_argument('--batch_size', type=int, default=64)
parser.add_argument('--epochs', type=int, default=10)
parser.add_argument('--lr', type=float, default=1e-3)
parser.add_argument('--num_members', type=int, default=3)
parser.add_argument('--channels', type=int, nargs='+', default=[32, 32])
parser.add_argument('--kernel_size', type=int, default=3)
parser.add_argument('--dropout', type=float, default=0.0)
parser.add_argument('--client_idx', type=int, default=0)
parser.add_argument('--debug', action='store_true')
args = parser.parse_args()

# --- Load data (select one client) ---
ds = load_dataset("tulipa762/electricity_load_diagrams", "uci", split="train")
values = torch.tensor(ds[args.client_idx]["target"], dtype=torch.float32)
if args.debug:
    values = values[:1000]

# Normalize
mean, std = values.mean(), values.std()
values = (values - mean) / std

# --- Create sequences ---
x_list, y_list = [], []
for i in range(len(values) - args.seq_len - args.horizon + 1):
    x_list.append(values[i:i + args.seq_len])
    y_list.append(values[i + args.seq_len:i + args.seq_len + args.horizon])

x = torch.stack(x_list).unsqueeze(1)  # (N, 1, SEQ_LEN)
y = torch.stack(y_list)                # (N, HORIZON)

# --- Split ---
n_train = int(0.8 * len(x))
train_ds = TensorDataset(x[:n_train], y[:n_train])
val_ds = TensorDataset(x[n_train:], y[n_train:])
train_loader = DataLoader(train_ds, batch_size=args.batch_size, shuffle=True)
val_loader = DataLoader(val_ds, batch_size=args.batch_size)

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

# --- Train ensemble ---
ensemble = DeepEnsemble(
    model_class=TCN,
    num_members=args.num_members,
    in_channels=1,
    num_channels_list=args.channels,
    kernel_size=args.kernel_size,
    output_head=DeterministicHead(in_features=args.channels[-1], out_features=args.horizon),
    dropout=args.dropout,
)
ensemble.to(device)

ensemble.fit(train_loader, val_loader, epochs=args.epochs, lr=args.lr)

# --- Inference ---
sample_x = x[:8].to(device)
outputs = ensemble.forward(sample_x)
for i, out in enumerate(outputs):
    print(f"Member {i}: {out.squeeze().tolist()}")
