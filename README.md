# Guideline for SSLVE

> Self-Supervised Latent Variable Evolution — Architecture Reference

---

## Contents

- [Data Flow — MAPElite](#data-flow--mapelite)
- [Data Flow — SSLVE](#data-flow--sslve)
- [Containment](#containment)
- [Core Methods](#core-methods)
- [Development Guide](#development-guide)

---

## Data Flow — MAPElite

```mermaid
flowchart LR
    START(("start"))

    subgraph SP["SearchPhase (SP)"]
        direction TB
        sample["sample(behavior_matching)"]
        sample -->|"θ"| make_agent["make_agent(θ)"]
        subgraph AG1["Agent (AG)"]
            agent1["agent"]
        end
        make_agent --> AG1
    end

    subgraph CO["Collector (CO)"]
        collect["collect(agent)"]
    end

    subgraph BM["BehaviorMatching (BM)"]
        direction TB
        subgraph BD["BehaviorDescriptor (BD)"]
            describe["describe(info)"]
            describe -->|"descriptor"| discretize["discretize(descriptor)"]
        end
        fitness_fn["fitness_fn(info)"]
        discretize -->|"bin_id"| update["update(thetas, infos)"]
        fitness_fn -->|"fitness"| update
    end

    START --> SP
    SP -->|"agent"| CO
    CO -->|"θ, info"| BM
    BM -.->|"self (dataset, bins_idx, ...)"| SP

    style SP fill:#fdf0ef,stroke:#c0392b,color:#000
    style CO fill:#eef5fb,stroke:#2471a3,color:#000
    style BM fill:#f5eefb,stroke:#7d3c98,color:#000
    style BD fill:#fdf0f8,stroke:#b03070,color:#000
    style AG1 fill:#fdf8ef,stroke:#b8860b,color:#000
```

---

## Data Flow — SSLVE

```mermaid
flowchart LR
    START(("start"))

    subgraph SP["SearchPhase (SP)"]
        direction TB
        sample["sample(latent_module, collector, behavior_matching)"]
        sample -->|"θ"| make_agent["make_agent(θ)"]
        subgraph AG2["Agent (AG)"]
            agent2["agent"]
        end
        make_agent --> AG2
    end

    subgraph CO["Collector (CO)"]
        collect2["collect(agent)"]
    end

    subgraph BM["BehaviorMatching (BM)"]
        direction TB
        subgraph BD2["BehaviorDescriptor (BD)"]
            describe2["describe(info)"]
            describe2 -->|"descriptor"| discretize2["discretize(descriptor)"]
        end
        fitness_fn2["fitness_fn(info)"]
        discretize2 -->|"bin_id"| update2["update(thetas, infos)"]
        fitness_fn2 -->|"fitness"| update2
    end

    subgraph LM["LatentModule (LM)"]
        fit["fit(dataset, bin_ids, bins_idx)"]
        fit --> AL1["AuxLoss₁"]
        fit --> AL2["AuxLoss₂"]
        fit --> ALn["AuxLoss ..."]
    end

    START --> SP
    SP -->|"agent"| CO
    CO -->|"θ, info"| BM
    BM -->|"dataset, bin_ids, bins_idx"| LM
    LM -.->|"self (encode, decode, ...)"| SP
    BM -.->|"self (dataset, bins_idx, ...)"| SP

    style SP fill:#fdf0ef,stroke:#c0392b,color:#000
    style CO fill:#eef5fb,stroke:#2471a3,color:#000
    style BM fill:#f5eefb,stroke:#7d3c98,color:#000
    style BD2 fill:#fdf0f8,stroke:#b03070,color:#000
    style LM fill:#eefbf2,stroke:#1e8449,color:#000
    style AG2 fill:#fdf8ef,stroke:#b8860b,color:#000
    style AL1 fill:#eefbf2,stroke:#1e8449,color:#000
    style AL2 fill:#eefbf2,stroke:#1e8449,color:#000
    style ALn fill:#eefbf2,stroke:#1e8449,color:#000
```

---

## Containment

```mermaid
flowchart TD
    ORCH["SSLVE / MAPElite\n(orchestrator)"]
    ORCH --> SP["SearchPhase (SP)"]
    ORCH --> CO["Collector (CO)"]
    ORCH --> BM["BehaviorMatching (BM)"]
    ORCH --> LM["LatentModule (LM)"]

    SP --> AG["Agent (AG)"]
    BM --> BD["BehaviorDescriptor (BD)"]
    LM --> AL["AuxLoss (AL)"]

    style ORCH fill:#fdf8ef,stroke:#b8860b,color:#000
    style SP fill:#fdf0ef,stroke:#c0392b,color:#000
    style CO fill:#eef5fb,stroke:#2471a3,color:#000
    style BM fill:#f5eefb,stroke:#7d3c98,color:#000
    style LM fill:#eefbf2,stroke:#1e8449,color:#000
    style AG fill:#fdf8ef,stroke:#b8860b,color:#000
    style BD fill:#fdf0f8,stroke:#b03070,color:#000
    style AL fill:#eefbf2,stroke:#1e8449,color:#000
```

---

## Core Methods

### SearchPhase (SP)

| Method | Signature | Returns |
|---|---|---|
| `sample` | `(**kwargs)` | `List[np.array]` — candidate θ vectors |
| `make_agent` | `(θ)` | `Agent` with weights set |

> Receives `latent_module`, `collector`, `behavior_matching` as kwargs. Uses or ignores depending on variant.

### Collector (CO)

| Method | Signature | Returns |
|---|---|---|
| `collect` | `(agent)` | `dict` — raw per-episode info |

### BehaviorMatching (BM)

| Method | Signature | Returns |
|---|---|---|
| `update` | `(thetas, infos)` | — |
| `coverage` | `()` | `float` |
| `fitness_stats` | `()` | `(min, mean, max)` |

**Exposed state** (read by SP and LM):

| Field | Type |
|---|---|
| `dataset` | `List[np.array]` |
| `bin_ids` | `List[bin_id]` |
| `bins_idx` | `dict{bin_id → [indices]}` |
| `fitnesses` | `List[float]` |
| `compute_rewards` | `bool` *(optional, default False)* |
| `rewards` | `List[float]` or `None` *(optional)* |

> When `compute_rewards` is True, `update()` populates `rewards` with a per-candidate scalar (e.g. 0.0 if not inserted, 1/(rank+1) if inserted). Recommended to support reward-based dynamic variation operator mixing in SP.

### LatentModule (LM)

| Method | Signature | Returns |
|---|---|---|
| `fit` | `(dataset, bin_ids, bins, ...)` | `history dict` |
| `encode` | `(x)` | `z` |
| `encode_dist` | `(x)` | `(μ, logvar)` |
| `decode` | `(z)` | `x̂` |

> `BaseBetaVAE` accepts `aux_losses=[(weight, AuxLoss), ...]` at construction. During `fit`, the base model builds a context dict and calls each auxiliary loss. `BetaVAE_SSLVE` is a legacy standalone implementation with built-in SSL loss.

### AuxLoss (AL) — supporting, inside LM

| Method | Signature | Returns |
|---|---|---|
| `compute` | `(**context)` | `scalar tensor` |

> Each AuxLoss has a `name` attribute used for logging. The context dict is base-model dependent; for `BaseBetaVAE` it includes: `x`, `x_recon`, `mu`, `logvar`, `z`, `batch_indices`, `bin_ids_batch`, `bins`, `dataset`, `model`.

### Agent (AG) — supporting, inside SP

| Method | Signature | Returns |
|---|---|---|
| `set_weights` | `(flat_weights)` | — |
| `act` | `(obs)` | `action` |
| `get_weight_dim` | `()` | `int` |

### BehaviorDescriptor (BD) — supporting, inside BM

| Method | Signature | Returns |
|---|---|---|
| `describe` | `(info)` | `descriptor` |
| `discretize` | `(descriptor)` | `bin_id` |
| `total_bins` | `()` | `int` |
| `bin_value` | `(bin_id)` | `np.array` *(recommended)* |

---

## Development Guide

### ① New task environment

| # | What to implement | Key methods |
|---|---|---|
| 1 | New **Collector (CO)** | `collect(agent) → info dict` |
| 2 | New **BehaviorDescriptor (BD)** | `describe(info)`, `discretize()`, `total_bins()` |
| 3 | New **Agent (AG)** *(if needed)* | `set_weights()`, `act()`, `get_weight_dim()` |

SP, BM, LM remain unchanged.

### ② New search / evolution method

| # | What to implement | Key methods |
|---|---|---|
| 1 | New **SearchPhase (SP)** | `sample(**kwargs)`, `make_agent(θ)` |

Must accept `latent_module`, `collector`, `behavior_matching` as kwargs (use or ignore). All other components unchanged.

### ③ Different behavior definition (same task)

| # | What to implement | Key methods |
|---|---|---|
| 1 | New **BehaviorDescriptor (BD)** | `describe(info)`, `discretize()`, `total_bins()` |

Same Collector (same info dict), just different BD extraction/discretization. Pass to BM constructor.

### ④ Different behavior matching / binning

| # | What to implement | Key methods |
|---|---|---|
| 1 | New **BehaviorMatching (BM)** | `update(thetas, infos)` |

Must expose `dataset`, `bin_ids`, `bins_idx`, `fitnesses`, `bins` for SP and LM to read. Contains a BD instance.

### ⑤ New auxiliary loss for latent module

| # | What to implement | Key methods |
|---|---|---|
| 1 | New **AuxLoss (AL)** | `compute(**context) → scalar tensor` |

Set `self.name` for logging. Picks needed keys from the context dict provided by the base model. Pass to `BaseBetaVAE` as `aux_losses=[(weight, aux)]`.

> If the auxiliary loss depends on behavior (e.g. bin center values), the BD should implement `bin_value(bin_id)`.

### ⑥ Reward-based dynamic operator mixing in SP

| # | What to implement | Key methods |
|---|---|---|
| 1 | **BM** with reward accounting | `update()` populates `self.rewards` when `self.compute_rewards` is True |
| 2 | **SP** with adaptive mixing | `sample()` reads `bm.rewards` to adjust operator allocation |

SP sets `bm.compute_rewards = True` when ready, then reads `bm.rewards` after each `update()` to adapt the variation operator mix.
