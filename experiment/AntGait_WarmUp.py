#@title AntGait PSE Warmup Checkpointing
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
W_BASE = 0.01  #@param {type:"number"}
W_PATH = 1.0  #@param {type:"number"}
TOP_K = 3  #@param {type:"integer"}
MAX_FITNESS = 0.0  #@param {type:"number"}

# --- Warmup Operators ---
USE_PSE_MUT = True  #@param {type:"boolean"}
USE_PSE_LINE = True  #@param {type:"boolean"}
PSE_MUT_SIGMA = 0.05  #@param {type:"number"}
GREEDY_MEM = True  #@param {type:"boolean"}

N_TOTAL = 500  #@param {type:"integer"}
EMA_ALPHA = 0.1  #@param {type:"number"}
TEMPERATURE = 10  #@param {type:"number"}
MIN_PROPORTION = 0.05  #@param {type:"number"}

# --- Checkpoint thresholds (archive sizes) ---
CHECKPOINT_THRESHOLDS = list(range(250, 20001, 250))  #@param

N_STEPS = 5000  #@param {type:"integer"}
SAVE_DIR = './checkpoints/'  #@param {type:"string"}

SEED = 42  #@param {type:"integer"}
random.seed(SEED)
np.random.seed(SEED)
torch.manual_seed(SEED)

# =============================================================================
# Setup
# =============================================================================
weight_dim = MLP_Agent(ARCHITECTURE, output_activation=OUTPUT_ACTIVATION).get_weight_dim()

fitness_fn = lambda info: W_BASE * (-(info['survival_sum'] - info['torque_sum'])) - W_PATH * info['path_length']

collector = AntOmniCollector(
    max_steps=MAX_STEPS,
    n_episodes=N_EPISODES,
    ctrl_cost_weight=CTRL_COST_WEIGHT,
    seed=SEED,
)
bd = AntGaitBD(bin_sizes=BIN_SIZES)
bm = MAPElitesBM(behavior_descriptor=bd, fitness_fn=fitness_fn, top_k=TOP_K, max_fitness=MAX_FITNESS)

# --- Build operators (PSE only) ---
operators = []
if USE_PSE_MUT:
    operators.append(PSEMut(sigma=PSE_MUT_SIGMA, greedy_mem=GREEDY_MEM))
if USE_PSE_LINE:
    operators.append(PSELine(greedy_mem=GREEDY_MEM))
assert len(operators) > 0, "At least one PSE operator must be enabled."

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

me = MAPElite(
    search_phase=sp,
    collector=collector,
    behavior_matching=bm,
)

# =============================================================================
# Run with checkpointing
# =============================================================================
print(f"Architecture: {ARCHITECTURE}")
print(f"Weight dim: {weight_dim}")
print(f"Output activation: {OUTPUT_ACTIVATION}")
print(f"Max steps: {MAX_STEPS}, Episodes: {N_EPISODES}")
print(f"Ctrl cost weight: {CTRL_COST_WEIGHT}")
print(f"Fitness weights: W_BASE={W_BASE}, W_PATH={W_PATH}")
print(f"Greedy member selection: {GREEDY_MEM}")
print(f"Gait BD grid: {BIN_SIZES} ({bd.total_bins()} bins)")
print(f"Samples per step: {N_TOTAL}")
print(f"Operators: {[op.name for op in operators]}")
print(f"Checkpoint thresholds: {CHECKPOINT_THRESHOLDS[0]}..{CHECKPOINT_THRESHOLDS[-1]} (step 250)")
print()

remaining_thresholds = sorted(CHECKPOINT_THRESHOLDS)

for t in range(N_STEPS):
    print(f"\n--- MAP-Elite Step {t+1}/{N_STEPS} ---")
    me.step()

    while remaining_thresholds and bm.archive_size() >= remaining_thresholds[0]:
        thresh = remaining_thresholds.pop(0)
        ckpt_path = f"{SAVE_DIR}archive_{bm.archive_size()}/"
        print(f"\n*** Checkpoint at archive size {bm.archive_size()} (threshold {thresh}) ***")
        save_checkpoint(ckpt_path, bm, me.history, sp=sp)
        print(f"    Saved to {ckpt_path}")

    if not remaining_thresholds:
        print("\nAll checkpoints saved. Stopping.")
        break

# =============================================================================
# Results
# =============================================================================
f_min, f_mean, f_max = bm.fitness_stats()
print(f"\nFinal archive size: {bm.archive_size()}")
print(f"Final coverage: {bm.coverage():.4f}")
print(f"Best fitness: {f_min:.4f}")
print(f"QD-score: {bm.qd_score():.4f}")

# =============================================================================
# Plot
# =============================================================================
me.plot_history()
