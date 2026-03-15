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
git clone -b Deep-Temporal-Uncertainty https://github.com/yugoguy/org.git adl_dev
```

To run dev_run code files:
```
cd adl_dev
```
```
python [dev_run file name]
```
