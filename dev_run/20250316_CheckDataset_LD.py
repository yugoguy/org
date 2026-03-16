import argparse
import pandas as pd

parser = argparse.ArgumentParser()
parser.add_argument('--data_path', type=str, default=None)
args = parser.parse_args()

if args.data_path is not None:
    df = pd.read_csv(args.data_path, sep=";", index_col=0, parse_dates=True, decimal=",")
else:
    import zipfile, io, urllib.request
    DATA_URL = "https://archive.ics.uci.edu/ml/machine-learning-databases/00321/LD2011_2014.txt.zip"
    with zipfile.ZipFile(io.BytesIO(urllib.request.urlopen(DATA_URL).read())) as z:
        df = pd.read_csv(z.open("LD2011_2014.txt"), sep=";", index_col=0, parse_dates=True, decimal=",")

df = df.resample("1h").sum()

print(f"Shape: {df.shape}")
print(f"Time range: {df.index[0]} to {df.index[-1]}")
print(f"\nPer-client stats:")
for i, col in enumerate(df.columns):
    vals = df[col]
    nonzero_pct = (vals != 0).mean() * 100
    print(f"  [{i:3d}] {col}: mean={vals.mean():.2f}, std={vals.std():.2f}, nonzero={nonzero_pct:.1f}%")
