# Core Library

- `Main.py` — Top-level QD loop (Algorithm 1).
- `LatentModules.py` — VAE architecture and training.
- `AuxLosses.py` — BehavePred auxiliary losses (prediction and mixup).
- `VariationOperators.py` — Parameter-space and latent-space variation operators.
- `SearchPhases.py` — Search phase logic and adaptive operator selection.
- `BehaviorDescriptors.py` — Behavior descriptor definitions.
- `BehaviorMatchings.py` — Archive binning and CVT/grid discretization.
- `Collectors.py` — Collect necessary information from the environments.
- `AgentModules.py` — MLP policy for the Ant task, joint angles for Planar Arm.
- `CMAME.py` — CMA-ME baseline implementation built on top of standard cma library.
- `ExperimentUtils.py` — Saving, Loading, metric utilities.
