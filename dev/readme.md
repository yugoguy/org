# Deep Temporal Uncertainty Framework

Uncertainty-aware deep learning framework for sequential prediction, supporting epistemic uncertainty estimation via Deep Ensembles and MC Dropout.

## Architecture

### Class Hierarchy

```
OutputHead                BaseModel                DeepEnsemble
├── forward(h)            ├── fit()                └── wraps N × BaseModel
└── loss(output, target)  ├── forward(x)
                          ├── mc_forward(x, num_mc)
                          └── extract_features(x)
```

### Components

**OutputHead** — Abstract output head defining `forward(h)` and `loss(output, target)`. Each subclass encapsulates both the prediction format and its corresponding loss function.

**BaseModel** — Abstract base for all backbone models. Provides:

- `fit(train_loader, val_loader, epochs, lr)` — full training loop
- `forward(x)` — extract features then pass through output head
- `mc_forward(x, num_mc)` — MC Dropout inference (runs forward `num_mc` times with dropout enabled, returns list of outputs)
- `extract_features(x)` — abstract, implemented by subclasses

**DeepEnsemble** — Wraps N independently initialized BaseModel instances. `fit()` trains each member sequentially. `forward()` returns a list of outputs (one per member).

## Epistemic Uncertainty Methods

### Deep Ensemble

Each ensemble member is a full model with independent random initialization. Diversity in predictions arises from different initializations converging to different local optima. Epistemic uncertainty is captured by the variance across members.

### MC Dropout

A single model with dropout > 0. At inference, `mc_forward` keeps dropout enabled and runs multiple stochastic forward passes. Variance across runs estimates epistemic uncertainty.

## File Structure

```
dev/
├── OutputHeads.py
├── BaseModels.py
└── DeepEnsemble.py

dev_run/
└── run.py
```
