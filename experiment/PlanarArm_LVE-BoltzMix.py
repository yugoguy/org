#@title PlanarArmCVT LVE (BetaVAE)
import numpy as np
import random
import torch

# =============================================================================
# Hyperparameters
# =============================================================================
N_JOINTS = 1000  #@param {type:"integer"}
END_EFFECTOR_DIM = 2  #@param {type:"integer"}
NOISE_SIGMA = 0.05  #@param {type:"number"}
N_NOISE_EPISODES = 3  #@param {type:"integer"}

N_BINS = 1950  #@param {type:"integer"}
CENTERS = "Precomputed_CVT_1950"  #@param ["Precomputed_CVT_1950", "CVT", "random"] {type:"string"}

FITNESS = "sine_dependency"  #@param ["angle_variance", "sine_dependency"] {type:"string"}
TOP_K = 3  #@param {type:"integer"}

# --- Variation Operators ---
USE_PSE_MUT = True  #@param {type:"boolean"}
USE_PSE_LINE = True  #@param {type:"boolean"}
USE_LVE_MUT = True  #@param {type:"boolean"}
USE_LVE_CROSS = True  #@param {type:"boolean"}
GREEDY_MEM = True  #@param {type:"boolean"}

PSE_MUT_SIGMA = 0.05  #@param {type:"number"}
LVE_MUT_SIGMA = 0.05  #@param {type:"number"}

# --- Warmup Operators ---
WARMUP_PSE_MUT = True  #@param {type:"boolean"}
WARMUP_PSE_LINE = True  #@param {type:"boolean"}

N_TOTAL = 200  #@param {type:"integer"}
WARMUP_THRESHOLD = 512  #@param {type:"integer"}
EMA_ALPHA = 0.3  #@param {type:"number"}
TEMPERATURE = 0.3  #@param {type:"number"}
MIN_PROPORTION = 0.05  #@param {type:"number"}

# --- Latent Module ---
LATENT_DIM = 32  #@param {type:"integer"}
HIDDEN_DIMS = [128]  #@param
BETA = 1e-2  #@param {type:"number"}
EPOCHS = 50  #@param {type:"integer"}
BATCH_SIZE = 512  #@param {type:"integer"}
LR = 1e-3  #@param {type:"number"}

N_STEPS = 1000  #@param {type:"integer"}

# --- Checkpoint ---
CHECKPOINT_PATH = ''  #@param {type:"string"}

SEED = 42  #@param {type:"integer"}
DEVICE = 'cuda' if torch.cuda.is_available() else 'cpu'

random.seed(SEED)
np.random.seed(SEED)
torch.manual_seed(SEED)

# =============================================================================
# Setup
# =============================================================================
ANGLES_PER_JOINT = END_EFFECTOR_DIM - 1
GENE_DIM = N_JOINTS * ANGLES_PER_JOINT

def _get_metric(info, key):
    v = info[key]
    return np.mean(v) if isinstance(v, list) else v

FITNESS_FNS = {
    'angle_variance': lambda info: _get_metric(info, 'angle_variance'),
    'sine_dependency': lambda info: _get_metric(info, 'sine_dependency'),
}
fitness_fn = FITNESS_FNS[FITNESS]

init_fn = lambda: np.random.uniform(-np.pi, np.pi, GENE_DIM)

collector = PlanarArmCollector(
    n_joints=N_JOINTS,
    end_effector_dim=END_EFFECTOR_DIM,
    noise_sigma=NOISE_SIGMA,
    n_episodes=N_NOISE_EPISODES,
)
bd = PlanarArmBD_CVT(n_bins=N_BINS, centers=CENTERS, bd_dim=END_EFFECTOR_DIM)
bm = MAPElitesBM(behavior_descriptor=bd, fitness_fn=fitness_fn, top_k=TOP_K, max_fitness=100)

# --- Build operators ---
operators = []
if USE_PSE_MUT:
    operators.append(PSEMut(sigma=PSE_MUT_SIGMA, greedy_mem=GREEDY_MEM))
if USE_PSE_LINE:
    operators.append(PSELine(greedy_mem=GREEDY_MEM))
if USE_LVE_MUT:
    operators.append(LVEMut(sigma=LVE_MUT_SIGMA, greedy_mem=GREEDY_MEM))
if USE_LVE_CROSS:
    operators.append(LVECross(greedy_mem=GREEDY_MEM))
assert len(operators) > 0, "At least one operator must be enabled."

# --- Build warmup operators ---
warmup_operators = []
if WARMUP_PSE_MUT:
    warmup_operators.append(PSEMut(sigma=PSE_MUT_SIGMA, greedy_mem=GREEDY_MEM))
if WARMUP_PSE_LINE:
    warmup_operators.append(PSELine(greedy_mem=GREEDY_MEM))
assert len(warmup_operators) > 0, "At least one warmup operator must be enabled."

# --- Build latent module ---
lm = BaseBetaVAE(
    input_dim=GENE_DIM,
    latent_dim=LATENT_DIM,
    hidden_dims=HIDDEN_DIMS,
    beta=BETA,
)

sp = BoltzmannMix(
    agent_class=PlanarArmAgent,
    architecture=GENE_DIM,
    operators=operators,
    warmup_operators=warmup_operators,
    n_total=N_TOTAL,
    warmup_threshold=WARMUP_THRESHOLD,
    ema_alpha=EMA_ALPHA,
    temperature=TEMPERATURE,
    min_proportion=MIN_PROPORTION,
    init_fn=init_fn,
)

orchestrator = SSLVE(
    search_phase=sp,
    collector=collector,
    behavior_matching=bm,
    latent_module=lm,
    device=DEVICE,
)

# --- Load checkpoint if specified ---
if CHECKPOINT_PATH:
    load_checkpoint(CHECKPOINT_PATH, bm, orchestrator.history, sp=sp, lm=lm, device=DEVICE)
    print(f"Loaded checkpoint from {CHECKPOINT_PATH}")
    print(f"Archive size: {bm.archive_size()}, Coverage: {bm.coverage():.4f}")

# =============================================================================
# Run
# =============================================================================
print(f"N joints: {N_JOINTS}")
print(f"End-effector dim: {END_EFFECTOR_DIM}")
print(f"Gene dim: {GENE_DIM}")
print(f"Noise sigma: {NOISE_SIGMA}, Episodes: {N_NOISE_EPISODES}")
print(f"Fitness: {FITNESS}")
print(f"Greedy member selection: {GREEDY_MEM}")
print(f"Latent dim: {LATENT_DIM}, Hidden: {HIDDEN_DIMS}, Beta: {BETA}")
print(f"Device: {DEVICE}")
print(f"N bins: {N_BINS} ({CENTERS})")
print(f"Samples per step: {N_TOTAL}")
print(f"Warmup threshold: {WARMUP_THRESHOLD}")
print(f"Operators: {[op.name for op in operators]}")
print(f"Warmup operators: {[op.name for op in warmup_operators]}")
print(f"Mode: SSLVE")
print()

train_kwargs = {
    'epochs': EPOCHS,
    'batch_size': BATCH_SIZE,
    'lr': LR,
    'verbose': True,
}
histories = orchestrator.run(n_steps=N_STEPS, train_kwargs=train_kwargs)

# =============================================================================
# Results
# =============================================================================
f_min, f_mean, f_max = bm.fitness_stats()
print(f"\nFinal archive size: {bm.archive_size()}")
print(f"Final coverage: {bm.coverage():.4f}")
print(f"Best fitness ({FITNESS}): {f_min:.4f}")
print(f"QD-score: {bm.qd_score():.4f}")

# =============================================================================
# Plot
# =============================================================================
orchestrator.plot_history()
sp.plot_allocation()
