# SSLVE Framework Map

> Self-Supervised Latent Variable Evolution — Architecture Reference

---

## Orchestrators

```mermaid
graph LR
    subgraph SSLVE
        direction LR
        S1[SP.sample] -->|"List[θ]"| S2["SP.make_agent → CO.collect"]
        S2 -->|"θ, info"| S3[BM.update]
        S3 -->|"dataset, bins"| S4[LM.fit]
        S4 -.->|"encoder/decoder"| S1
    end

    subgraph MAPElite
        direction LR
        M1[SP.sample] -->|"List[θ]"| M2["SP.make_agent → CO.collect"]
        M2 -->|"θ, info"| M3[BM.update]
    end
```

---

## Core Components — Data Flow

```mermaid
flowchart LR
    SP["<b>SearchPhase (SP)</b><br/>Generates candidate θ vectors.<br/>Selects parents from archive,<br/>applies variation in parameter<br/>or latent space."]
    CO["<b>Collector (CO)</b><br/>Runs agent in environment.<br/>Returns raw per-episode<br/>measurements as info dict."]
    BM["<b>BehaviorMatching (BM)</b><br/>Computes BD, evaluates fitness,<br/>maintains top-k archive per bin.<br/>Contains BehaviorDescriptor."]
    LM["<b>LatentModule (LM)</b><br/>Trains representation on<br/>archive θ vectors. Provides<br/>encode/decode for SP."]

    SP -->|"List[θ] → agent"| CO
    CO -->|"θ, info"| BM
    BM -->|"dataset, bins"| LM
    LM -.->|"encoder / decoder"| SP

    style SP fill:#fdf0ef,stroke:#c0392b,color:#000
    style CO fill:#eef5fb,stroke:#2471a3,color:#000
    style BM fill:#f5eefb,stroke:#7d3c98,color:#000
    style LM fill:#eefbf2,stroke:#1e8449,color:#000
```

---

## Containment

```mermaid
flowchart TD
    SSLVE_O["<b>SSLVE / MAPElite</b><br/>(orchestrator)"]
    SSLVE_O --> SP
    SSLVE_O --> CO
    SSLVE_O --> BM
    SSLVE_O --> LM

    SP["SearchPhase (SP)"] --> AG["Agent class"]
    BM["BehaviorMatching (BM)"] --> BD["BehaviorDescriptor (BD)"]

    style SSLVE_O fill:#fdf8ef,stroke:#b8860b,color:#000
    style SP fill:#fdf0ef,stroke:#c0392b,color:#000
    style CO fill:#eef5fb,stroke:#2471a3,color:#000
    style BM fill:#f5eefb,stroke:#7d3c98,color:#000
    style LM fill:#eefbf2,stroke:#1e8449,color:#000
    style AG fill:#fdf8ef,stroke:#b8860b,color:#000
    style BD fill:#fdf0f8,stroke:#b03070,color:#000
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

### LatentModule (LM)

| Method | Signature | Returns |
|---|---|---|
| `fit` | `(dataset, bin_ids, bins, ...)` | `history dict` |
| `encode` | `(x)` | `z` |
| `encode_dist` | `(x)` | `(μ, logvar)` |
| `decode` | `(z)` | `x̂` |

### Agent (supporting — inside SP)

| Method | Signature | Returns |
|---|---|---|
| `set_weights` | `(flat_weights)` | — |
| `act` | `(obs)` | `action` |
| `get_weight_dim` | `()` | `int` |

### BehaviorDescriptor (BD) (supporting — inside BM)

| Method | Signature | Returns |
|---|---|---|
| `describe` | `(info)` | `descriptor` |
| `discretize` | `(descriptor)` | `bin_id` |
| `total_bins` | `()` | `int` |

---

## Swap Guide

### ① New task environment

| # | What to implement | Key methods |
|---|---|---|
| 1 | New **Collector** | `collect(agent) → info dict` |
| 2 | New **BehaviorDescriptor** | `describe(info)`, `discretize()`, `total_bins()` |
| 3 | New **Agent** *(if needed)* | `set_weights()`, `act()`, `get_weight_dim()` |

SP, BM, LM remain unchanged.

### ② New search / evolution method

| # | What to implement | Key methods |
|---|---|---|
| 1 | New **SearchPhase** | `sample(**kwargs)`, `make_agent(θ)` |

Must accept `latent_module`, `collector`, `behavior_matching` as kwargs (use or ignore). All other components unchanged.

### ③ Different behavior definition (same task)

| # | What to implement | Key methods |
|---|---|---|
| 1 | New **BehaviorDescriptor** | `describe(info)`, `discretize()`, `total_bins()` |

Same Collector (same info dict), just different BD extraction/discretization. Pass to BM constructor.

### ④ Different behavior matching / binning

| # | What to implement | Key methods |
|---|---|---|
| 1 | New **BehaviorMatching** | `update(thetas, infos)` |

Must expose `dataset`, `bin_ids`, `bins_idx`, `fitnesses`, `bins` for SP and LM to read. Contains a BD instance.
