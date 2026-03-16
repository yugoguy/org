# Deep Temporal Uncertainty Framework

> Uncertainty-aware sequential prediction with modular epistemic uncertainty estimation.

---

## Contents

- [Data Flow — Deep Ensemble](#data-flow--deep-ensemble)
- [Data Flow — MC Dropout](#data-flow--mc-dropout)
- [Containment](#containment)
- [Core Methods](#core-methods)

---

## Data Flow — Deep Ensemble

```mermaid
flowchart LR
    START(("start"))

    subgraph DE["DeepEnsemble (DE)"]
        direction TB
        subgraph M1["BaseModel (member 1)"]
            direction TB
            ef1["extract_features(x)"]
            ef1 -->|"h"| oh1["OutputHead"]
        end
        subgraph M2["BaseModel (member 2)"]
            direction TB
            ef2["extract_features(x)"]
            ef2 -->|"h"| oh2["OutputHead"]
        end
        subgraph MN["BaseModel (member N)"]
            direction TB
            efN["extract_features(x)"]
            efN -->|"h"| ohN["OutputHead"]
        end
    end

    START -->|"x"| DE
    oh1 -->|"output 1"| VAR["variance across members"]
    oh2 -->|"output 2"| VAR
    ohN -->|"output N"| VAR
    VAR --> EP["epistemic uncertainty"]

    style DE fill:#f5eefb,stroke:#7d3c98,color:#000
    style M1 fill:#eef5fb,stroke:#2471a3,color:#000
    style M2 fill:#eef5fb,stroke:#2471a3,color:#000
    style MN fill:#eef5fb,stroke:#2471a3,color:#000
    style VAR fill:#fdf8ef,stroke:#b8860b,color:#000
    style EP fill:#eefbf2,stroke:#1e8449,color:#000
```

---

## Data Flow — MC Dropout

```mermaid
flowchart LR
    START(("start"))

    subgraph BM["BaseModel - dropout ON"]
        direction TB
        r1["run 1 - random mask"]
        r1 -->|"h"| oh1["OutputHead"]
        r2["run 2 - random mask"]
        r2 -->|"h"| oh2["OutputHead"]
        rN["run N - random mask"]
        rN -->|"h"| ohN["OutputHead"]
    end

    START -->|"x, num_mc"| BM
    oh1 -->|"output 1"| VAR["variance across runs"]
    oh2 -->|"output 2"| VAR
    ohN -->|"output N"| VAR
    VAR --> EP["epistemic uncertainty"]

    style BM fill:#eef5fb,stroke:#2471a3,color:#000
    style VAR fill:#fdf8ef,stroke:#b8860b,color:#000
    style EP fill:#eefbf2,stroke:#1e8449,color:#000
```

---

## Containment

```mermaid
flowchart TD
    DE["DeepEnsemble\n(orchestrator)"]
    DE --> BM["BaseModel"]
    BM --> OH["OutputHead"]

    style DE fill:#f5eefb,stroke:#7d3c98,color:#000
    style BM fill:#eef5fb,stroke:#2471a3,color:#000
    style OH fill:#fdf0ef,stroke:#c0392b,color:#000
```

---

## Core Methods

### BaseModel

| Method | Signature | Returns |
|---|---|---|
| `extract_features` | `(x)` | `h` — hidden representation |
| `forward` | `(x)` | output from OutputHead |
| `mc_forward` | `(x, num_mc)` | `list[output]` — num_mc stochastic passes |
| `fit` | `(train_loader, val_loader, epochs, lr)` | — |

> `extract_features` is abstract, implemented by subclasses. `mc_forward` keeps dropout enabled during inference and runs `forward` num_mc times, each with a different random dropout mask.

### OutputHead

| Method | Signature | Returns |
|---|---|---|
| `forward` | `(h)` | prediction output |
| `loss` | `(output, target)` | scalar loss |

> Each subclass defines both the output format and its corresponding loss function. The loss is called by BaseModel during `fit`.

### DeepEnsemble

| Method | Signature | Returns |
|---|---|---|
| `forward` | `(x)` | `list[output]` — one per member |
| `fit` | `(train_loader, val_loader, epochs, lr)` | — |
| `to` | `(device)` | `self` |

> Creates N independently initialized BaseModel instances. `fit` trains each member sequentially. `forward` collects outputs from all members.

---
