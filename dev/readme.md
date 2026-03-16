# Deep Temporal Uncertainty Framework

Uncertainty-aware deep learning framework for sequential prediction, supporting epistemic uncertainty estimation via Deep Ensembles and MC Dropout.

## Architecture

```mermaid
classDiagram
    class OutputHead {
        <<abstract>>
        +forward(h) output
        +loss(output, target) scalar
    }

    class BaseModel {
        <<abstract>>
        +output_head: OutputHead
        +fit(train_loader, val_loader, epochs, lr)
        +forward(x) output
        +mc_forward(x, num_mc) list[output]
        +extract_features(x)* hidden
    }

    class DeepEnsemble {
        +members: list[BaseModel]
        +fit(train_loader, val_loader, epochs, lr)
        +forward(x) list[output]
        +to(device)
    }

    BaseModel *-- OutputHead : has-a
    DeepEnsemble o-- BaseModel : wraps N ×
```

## Epistemic Uncertainty Methods

### Deep Ensemble

Each ensemble member is a full model with independent random initialization. Diversity in predictions arises from different initializations converging to different local optima. Epistemic uncertainty is captured by the variance across members.

```mermaid
flowchart LR
    X[Input x] --> M1[Member 1\ninit 1]
    X --> M2[Member 2\ninit 2]
    X --> MN[Member N\ninit N]
    M1 --> O1[Output 1]
    M2 --> O2[Output 2]
    MN --> ON[Output N]
    O1 --> V[Variance → epistemic\nuncertainty]
    O2 --> V
    ON --> V
```

### MC Dropout

A single model with dropout > 0. At inference, `mc_forward` keeps dropout enabled and runs multiple stochastic forward passes. Variance across runs estimates epistemic uncertainty.

```mermaid
flowchart LR
    X[Input x] --> R1[Run 1\nrandom mask]
    X --> R2[Run 2\nrandom mask]
    X --> RN[Run N\nrandom mask]
    subgraph Model — dropout ON
        R1
        R2
        RN
    end
    R1 --> O1[Output 1]
    R2 --> O2[Output 2]
    RN --> ON[Output N]
    O1 --> V[Variance → epistemic\nuncertainty]
    O2 --> V
    ON --> V
```

## File Structure

```
dev/
├── OutputHeads.py
├── BaseModels.py
└── DeepEnsemble.py

dev_run/
└── run.py
```
