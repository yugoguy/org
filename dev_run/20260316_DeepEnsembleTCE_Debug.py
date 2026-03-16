## If you don't have dataset downloaded to your directory where git is cloned
# python 20260316_DeepEnsembleTCE_Debug.py --batch_size 256 --epochs 20 --lr 1e-3 --debug
## To download and to use the downloaded dataset
# wget https://archive.ics.uci.edu/ml/machine-learning-databases/00321/LD2011_2014.txt.zip && unzip LD2011_2014.txt.zip
# python 20260316_DeepEnsembleTCE_Debug.py --data_path ../data/LD2011_2014.txt --batch_size 256 --epochs 20 --lr 1e-3 --debug

import sys
sys.path.append('../dev')

import argparse
import pandas as pd
import torch
from torch.utils.data import DataLoader, TensorDataset
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
parser.add_argument('--channels', type=int, nargs='+', default=[64, 64, 64])
parser.add_argument('--kernel_size', type=int, default=5)
parser.add_argument('--dropout', type=float, default=0.0)
parser.add_argument('--client_idx', type=int, default=0)
parser.add_argument('--data_path', type=str, default=None, help='Path to local LD2011_2014.txt')
parser.add_argument('--debug', action='store_true')
args = parser.parse_args()

# --- Load data (select one client) ---
if args.data_path is not None:
    df = pd.read_csv(args.data_path, sep=";", index_col=0, parse_dates=True, decimal=",")
else:
    import zipfile, io, urllib.request
    DATA_URL = "https://archive.ics.uci.edu/ml/machine-learning-databases/00321/LD2011_2014.txt.zip"
    with zipfile.ZipFile(io.BytesIO(urllib.request.urlopen(DATA_URL).read())) as z:
        df = pd.read_csv(z.open("LD2011_2014.txt"), sep=";", index_col=0, parse_dates=True, decimal=",")
df = df.resample("1h").sum()
values = torch.tensor(df.iloc[:, args.client_idx].values, dtype=torch.float32)
if args.debug:
    values = values[:1000]

# Normalize
mean, std = values.mean(), values.std()
if std < 1e-8:
    raise ValueError(f"Client {args.client_idx} has near-zero std ({std:.2e}). Try a different --client_idx.")
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
    output_head_class=DeterministicHead,
    output_head_kwargs=dict(in_features=args.channels[-1], out_features=args.horizon),
    dropout=args.dropout,
)
ensemble.to(device)

ensemble.fit(train_loader, val_loader, epochs=args.epochs, lr=args.lr)

# --- Inference ---
sample_x = x[:8].to(device)
outputs = ensemble.forward(sample_x)
for i, out in enumerate(outputs):
    print(f"Member {i}: {out.squeeze().tolist()}")
