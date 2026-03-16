This branch contains some mid-development codes for ADL Group Project.

dev directory contains some classes and functions.

dev_run directory contains minimal codes to run models.

The codes are not optimized for submission yet.

Assumes following enviornment:

```
micromamba create --name comp0197-pt python=3.12 -y && micromamba activate comp0197-pt && pip install torch torchvision pillow --index-url https://download.pytorch.org/whl/cpu
```

```
micromamba activate comp0197-pt
```

```
Package            Version     Build
------------------ ----------- -----
aiohappyeyeballs   2.6.1
aiohttp            3.13.3
aiosignal          1.4.0
annotated-doc      0.0.4
anyio              4.12.1
attrs              25.4.0
certifi            2026.2.25
charset-normalizer 3.4.6
click              8.3.1
dill               0.4.0
filelock           3.20.0
frozenlist         1.8.0
fsspec             2025.12.0
h11                0.16.0
hf-xet             1.4.2
httpcore           1.0.9
httpx              0.28.1
huggingface_hub    1.7.1
idna               3.11
Jinja2             3.1.6
markdown-it-py     4.0.0
MarkupSafe         3.0.2
mdurl              0.1.2
mpmath             1.3.0
multidict          6.7.1
multiprocess       0.70.18
networkx           3.6.1
numpy              2.3.5
packaging          26.0
pandas             3.0.1
pillow             12.0.0
pip                26.0.1
propcache          0.4.1
pyarrow            23.0.1
Pygments           2.19.2
python-dateutil    2.9.0.post0
PyYAML             6.0.3
requests           2.32.5
rich               14.3.3
setuptools         82.0.0
shellingham        1.5.4
six                1.17.0
sympy              1.14.0
torch              2.10.0      2
torchvision        0.25.0
tqdm               4.67.3
typer              0.24.1
typing_extensions  4.15.0
urllib3            2.6.3
wheel              0.46.3
xxhash             3.6.0
yarl               1.23.0
```

Gitclone
```
rm -rf adl_dev && git clone -b Deep-Temporal-Uncertainty https://github.com/yugoguy/org.git adl_dev
```

To run dev_run code files:
```
cd adl_dev/dev_run
```
```
python [dev_run file name].py ---[args]
```

Quick set up for me
```
cd && cd Downloads && rm -rf adl_dev && git clone -b Deep-Temporal-Uncertainty https://github.com/yugoguy/org.git adl_dev && cd adl_dev/dev_run
```
