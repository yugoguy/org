#@title AntOmniGrid PSE Warmup Checkpointing
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
CTRL_COST_WEIGHT = 0.1  #@param {type:"number"}
BIN_RANGES = [(-100.0, 100.0), (-100.0, 100.0)]  #@param
BIN_SIZES = [100, 100]  #@param
TOP_K = 3  #@param {type:"integer"}

# --- Fitness Weights ---
W_PATH = 1.0  #@param {type:"number"}
W_ANGLE_VAR = 0.0  #@param {type:"number"}

# --- Warmup Operators ---
USE_PSE_MUT = True  #@param {type:"boolean"}
USE_PSE_LINE = True  #@param {type:"boolean"}
PSE_MUT_SIGMA = 0.01  #@param {type:"number"}
GREEDY_MEM = True  #@param {type:"boolean"}

N_TOTAL = 500  #@param {type:"integer"}
EMA_ALPHA = 0.3  #@param {type:"number"}
TEMPERATURE = 0.5  #@param {type:"number"}
MIN_PROPORTION = 0.05  #@param {type:"number"}

# --- Checkpoint thresholds (archive sizes) ---
CHECKPOINT_THRESHOLDS = list(range(250, 10001, 250))  #@param

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

fitness_fn = lambda info: -(info['survival_sum'] - info['torque_sum']) - W_PATH * info['path_length'] + W_ANGLE_VAR * info['heading_angle_var']

collector = AntOmniCollector(
    max_steps=MAX_STEPS,
    n_episodes=N_EPISODES,
    ctrl_cost_weight=CTRL_COST_WEIGHT,
    seed=SEED,
)
bd = AntOmniBD_Grid(bin_ranges=BIN_RANGES, bin_sizes=BIN_SIZES)
bm = MAPElitesBM(behavior_descriptor=bd, fitness_fn=fitness_fn, top_k=TOP_K)

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
print(f"Grid: {BIN_SIZES[0]}x{BIN_SIZES[1]}, Range: {BIN_RANGES}")
print(f"Fitness weights: W_PATH={W_PATH}, W_ANGLE_VAR={W_ANGLE_VAR}")
print(f"Greedy member selection: {GREEDY_MEM}")
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
print(f"Final coverage: {bm.coverage():.6f}")
print(f"Best fitness: {f_min:.4f}")
print(f"QD-score: {bm.qd_score():.4f}")

# =============================================================================
# Plot
# =============================================================================
me.plot_history()
#sp.plot_allocation()
