#@title AntGaitUni MAP-Elites
import numpy as np
import random
import torch

# =============================================================================
# Hyperparameters
# =============================================================================
ARCHITECTURE = [27, 64, 64, 8]  #@param
OUTPUT_ACTIVATION = 'tanh'  #@param {type:"string"}
MAX_STEPS = 500  #@param {type:"integer"}
N_EPISODES = 2  #@param {type:"integer"}
CTRL_COST_WEIGHT = 0.5  #@param {type:"number"}

# --- Gait BD Grid ---
BIN_SIZES = [10, 10, 10, 10]  #@param

# --- Fitness ---
TOP_K = 1  #@param {type:"integer"}
MAX_FITNESS = 10.0  #@param {type:"number"}

# --- Variation Operators ---
USE_PSE_MUT = True  #@param {type:"boolean"}
USE_PSE_LINE = True  #@param {type:"boolean"}
GREEDY_MEM = True  #@param {type:"boolean"}
PSE_MUT_SIGMA = 0.05  #@param {type:"number"}

N_TOTAL = 500  #@param {type:"integer"}
EMA_ALPHA = 0.1  #@param {type:"number"}
TEMPERATURE = 1  #@param {type:"number"}
MIN_PROPORTION = 0.05  #@param {type:"number"}

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

# --- Build operators ---
operators = []
if USE_PSE_MUT:
    operators.append(PSEMut(sigma=PSE_MUT_SIGMA, greedy_mem=GREEDY_MEM))
if USE_PSE_LINE:
    operators.append(PSELine(greedy_mem=GREEDY_MEM))
assert len(operators) > 0, "At least one operator must be enabled."

sp = BoltzmannMix(
    agent_class=MLP_Agent,
    architecture=ARCHITECTURE,
    agent_kwargs={'output_activation': OUTPUT_ACTIVATION},
    operators=operators,
    n_total=N_TOTAL,
    warmup_threshold=999999,
    ema_alpha=EMA_ALPHA,
    temperature=TEMPERATURE,
    min_proportion=MIN_PROPORTION,
)

orchestrator = MAPElite(
    search_phase=sp,
    collector=collector,
    behavior_matching=bm,
)

if CHECKPOINT_PATH:
    load_checkpoint(CHECKPOINT_PATH, bm, orchestrator.history, sp=sp)
    print(f"Loaded checkpoint from {CHECKPOINT_PATH}")
    print(f"Archive size: {bm.archive_size()}, Coverage: {bm.coverage():.4f}")

# =============================================================================
# Run
# =============================================================================
print(f"Architecture: {ARCHITECTURE}")
print(f"Weight dim: {weight_dim}")
print(f"Fitness: -forward_sum")
print(f"Gait BD grid: {BIN_SIZES} ({bd.total_bins()} bins)")
print(f"Samples per step: {N_TOTAL}")
print(f"Operators: {[op.name for op in operators]}")
print(f"Mode: MAPElite")
print()

orchestrator.run(n_steps=N_STEPS)

f_min, f_mean, f_max = bm.fitness_stats()
print(f"\nFinal archive size: {bm.archive_size()}")
print(f"Final coverage: {bm.coverage():.4f}")
print(f"Best fitness: {f_min:.4f}")
print(f"QD-score: {bm.qd_score():.4f}")

orchestrator.plot_history()
sp.plot_allocation()
