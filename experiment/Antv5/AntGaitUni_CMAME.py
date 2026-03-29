#@title AntGaitUni CMA-ME
import numpy as np
import random
import torch

# =============================================================================
# Hyperparameters
# =============================================================================
ARCHITECTURE = [27, 64, 64, 8]  #@param
OUTPUT_ACTIVATION = 'tanh'  #@param {type:"string"}
MAX_STEPS = 500  #@param {type:"integer"}
N_EPISODES = 3  #@param {type:"integer"}
CTRL_COST_WEIGHT = 0.5  #@param {type:"number"}

# --- Gait BD Grid ---
BIN_SIZES = [10, 10, 10, 10]  #@param

# --- Fitness ---
TOP_K = 1  #@param {type:"integer"}
MAX_FITNESS = 10.0  #@param {type:"number"}

# --- CMA-ME ---
N_EMITTERS = 10  #@param {type:"integer"}
SIGMA_INIT = 0.05  #@param {type:"number"}
POPSIZE = 50  #@param {type:"integer"}
GREEDY_MEM = True  #@param {type:"boolean"}
SEPARABLE = True  #@param {type:"boolean"}
N_INIT_SAMPLES = 500  #@param {type:"integer"}

N_STEPS = 1000  #@param {type:"integer"}

# --- Checkpoint ---
CHECKPOINT_PATH = ''  #@param {type:"string"}

SEED = 42  #@param {type:"integer"}

random.seed(SEED)
np.random.seed(SEED)
torch.manual_seed(SEED)

# =============================================================================
# Setup
# =============================================================================
weight_dim = MLP_Agent(ARCHITECTURE, output_activation=OUTPUT_ACTIVATION).get_weight_dim()

fitness_fn = lambda info: -info['forward_sum']

collector = AntOmniCollector(
    max_steps=MAX_STEPS,
    n_episodes=N_EPISODES,
    ctrl_cost_weight=CTRL_COST_WEIGHT,
    seed=SEED,
)
bd = AntGaitBD(bin_sizes=BIN_SIZES)
bm = MAPElitesBM(behavior_descriptor=bd, fitness_fn=fitness_fn, top_k=TOP_K, max_fitness=MAX_FITNESS)

orchestrator = CMAME(
    agent_class=MLP_Agent,
    architecture=ARCHITECTURE,
    agent_kwargs={'output_activation': OUTPUT_ACTIVATION},
    collector=collector,
    behavior_matching=bm,
    n_emitters=N_EMITTERS,
    sigma_init=SIGMA_INIT,
    popsize=POPSIZE,
    greedy_mem=GREEDY_MEM,
    separable=SEPARABLE,
    n_init_samples=N_INIT_SAMPLES,
)

if CHECKPOINT_PATH:
    load_cmame_checkpoint(CHECKPOINT_PATH, orchestrator)
    print(f"Loaded checkpoint from {CHECKPOINT_PATH}")
    print(f"Archive size: {bm.archive_size()}, Coverage: {bm.coverage():.4f}")

# =============================================================================
# Run
# =============================================================================
print(f"Architecture: {ARCHITECTURE}")
print(f"Weight dim: {weight_dim}")
print(f"Fitness: -forward_sum")
print(f"Gait BD grid: {BIN_SIZES} ({bd.total_bins()} bins)")
print(f"N emitters: {N_EMITTERS}, Popsize: {POPSIZE}, Sigma: {SIGMA_INIT}")
print(f"Evals per step: {N_EMITTERS * POPSIZE}")
print(f"Init samples: {N_INIT_SAMPLES}")
print(f"Mode: CMAME")
print()

orchestrator.run(n_steps=N_STEPS)

f_min, f_mean, f_max = bm.fitness_stats()
print(f"\nFinal archive size: {bm.archive_size()}")
print(f"Final coverage: {bm.coverage():.4f}")
print(f"Best fitness: {f_min:.4f}")
print(f"QD-score: {bm.qd_score():.4f}")
print(f"Total evaluations: {orchestrator.total_evals}")

orchestrator.plot_history()
