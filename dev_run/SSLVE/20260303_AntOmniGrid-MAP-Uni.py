#@title Ant Omni-directional MAP-Elite PSE
import numpy as np
import random
import torch

# =============================================================================
# Hyperparameters
# =============================================================================
ARCHITECTURE = [87, 64, 64, 8]
OUTPUT_ACTIVATION = 'tanh'

MAX_STEPS = 500
N_EPISODES = 2

BIN_RANGES = [(-100.0, 100.0), (-100.0, 100.0)]
BIN_SIZES = [100, 100]

TOP_K = 3
N_SAMPLES = 200
MUTATION_SIGMA = 0.1

N_STEPS = 100

SEED = 42

random.seed(SEED)
np.random.seed(SEED)
torch.manual_seed(SEED)

# =============================================================================
# Setup
# =============================================================================
weight_dim = MLP_Agent(ARCHITECTURE, output_activation=OUTPUT_ACTIVATION).get_weight_dim()

fitness_fn = lambda info: -(info['survival_sum'] - info['torque_sum'])

collector = AntOmniCollector(
    max_steps=MAX_STEPS,
    n_episodes=N_EPISODES,
    seed=SEED,
)
bd = AntOmniBD_Grid(bin_ranges=BIN_RANGES, bin_sizes=BIN_SIZES)
bm = MAPElitesBM(behavior_descriptor=bd, fitness_fn=fitness_fn, top_k=TOP_K)

sp = UniBinUniMemPSE(
    agent_class=MLP_Agent,
    architecture=ARCHITECTURE,
    agent_kwargs={'output_activation': OUTPUT_ACTIVATION},
    mutation_sigma=MUTATION_SIGMA,
    n_samples=N_SAMPLES,
)

me = MAPElite(
    search_phase=sp,
    collector=collector,
    behavior_matching=bm,
)

# =============================================================================
# Run
# =============================================================================
print(f"Architecture: {ARCHITECTURE}")
print(f"Weight dim: {weight_dim}")
print(f"Output activation: {OUTPUT_ACTIVATION}")
print(f"Max steps: {MAX_STEPS}, Episodes: {N_EPISODES}")
print(f"Grid: {BIN_SIZES[0]}x{BIN_SIZES[1]}, Range: {BIN_RANGES}")
print(f"Samples per step: {N_SAMPLES}")
print(f"Mutation sigma: {MUTATION_SIGMA}")
print()

me.run(n_steps=N_STEPS)

# =============================================================================
# Results
# =============================================================================
f_min, f_mean, f_max = bm.fitness_stats()
print(f"\nFinal archive size: {bm.archive_size()}")
print(f"Final coverage: {bm.coverage():.6f}")
print(f"Best fitness: {f_min:.4f}")

# =============================================================================
# Plot
# =============================================================================
me.plot_history()
