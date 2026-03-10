#@title GenPlanarArmCVT SSLVE BoltzmannMix Flow
import numpy as np
import random
import torch

# =============================================================================
# Hyperparameters
# =============================================================================
N_JOINTS = 100  #@param {type:"integer"}
END_EFFECTOR_DIM = 2  #@param {type:"integer"}
NOISE_SIGMA = 0.05  #@param {type:"number"}
N_NOISE_EPISODES = 3  #@param {type:"integer"}

N_BINS = 1950  #@param {type:"integer"}
CENTERS = "Precomputed_CVT_1950"  #@param ["Precomputed_CVT_1950", "CVT", "random"] {type:"string"}

FITNESS = "angle_variance"  #@param ["angle_variance", "local_abs_dependency", "sine_dependency"] {type:"string"}
TOP_K = 3  #@param {type:"integer"}

# --- Variation Operators ---
USE_PSE_MUT = True  #@param {type:"boolean"}
USE_PSE_LINE = True  #@param {type:"boolean"}
USE_LVE_MUT = True  #@param {type:"boolean"}
USE_LVE_CROSS = True  #@param {type:"boolean"}
USE_STD_SUPPORT_LVE = True  #@param {type:"boolean"}

PSE_MUT_SIGMA = 0.3  #@param {type:"number"}
LVE_MUT_SIGMA = 0.05  #@param {type:"number"}
STD_SUPPORT_LO = -2.0  #@param {type:"number"}
STD_SUPPORT_HI = 2.0  #@param {type:"number"}

# --- Warmup Operators ---
WARMUP_PSE_MUT = True  #@param {type:"boolean"}
WARMUP_PSE_LINE = True  #@param {type:"boolean"}

N_TOTAL = 200  #@param {type:"integer"}
WARMUP_THRESHOLD = 512  #@param {type:"integer"}
EMA_ALPHA = 0.3  #@param {type:"number"}
TEMPERATURE = 0.5  #@param {type:"number"}
MIN_PROPORTION = 0.05  #@param {type:"number"}

# --- Latent Module ---
USE_FLOW_PRIOR = True  #@param {type:"boolean"}
LATENT_DIM = 10  #@param {type:"integer"}
HIDDEN_DIMS = [32]  #@param
BETA = 1e-2  #@param {type:"number"}
NUM_FLOWS = 3  #@param {type:"integer"}
FLOW_HIDDEN = 16  #@param {type:"integer"}
FLOW_HIDDEN_LAYERS = 2  #@param {type:"integer"}
EPOCHS = 100  #@param {type:"integer"}
BATCH_SIZE = 512  #@param {type:"integer"}
LR = 1e-3  #@param {type:"number"}

# --- Aux Losses ---
USE_BIN_PRED = False  #@param {type:"boolean"}
GAMMA_BIN_PRED = 1e-3  #@param {type:"number"}

N_STEPS = 500  #@param {type:"integer"}

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

FITNESS_FNS = {
    'angle_variance': lambda info: np.mean(info['angle_variance']) if isinstance(info['angle_variance'], list) else info['angle_variance'],
    'local_abs_dependency': lambda info: np.mean(info['local_abs_dependency']) if isinstance(info['local_abs_dependency'], list) else info['local_abs_dependency'],
    'sine_dependency': lambda info: np.mean(info['sine_dependency']) if isinstance(info['sine_dependency'], list) else info['sine_dependency'],
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
bm = MAPElitesBM(behavior_descriptor=bd, fitness_fn=fitness_fn, top_k=TOP_K)

# --- Build operators ---
operators = []
translate_fn = None
if USE_PSE_MUT:
    operators.append(UniBinUniMemPSEMut(sigma=PSE_MUT_SIGMA))
if USE_PSE_LINE:
    operators.append(UniBinUniMemPSELine())
if USE_LVE_MUT:
    operators.append(UniBinUniMemLVEMut(sigma=LVE_MUT_SIGMA))
if USE_LVE_CROSS:
    operators.append(UniBinUniMemLVECross())
assert len(operators) > 0, "At least one variation operator must be enabled."

# --- Build warmup operators ---
warmup_operators = []
if WARMUP_PSE_MUT:
    warmup_operators.append(UniBinUniMemPSEMut(sigma=PSE_MUT_SIGMA))
if WARMUP_PSE_LINE:
    warmup_operators.append(UniBinUniMemPSELine())
assert len(warmup_operators) > 0, "At least one warmup operator must be enabled."

# --- Build aux losses ---
aux_losses = []
if USE_BIN_PRED:
    aux = BinPred(behavior_descriptor=bd, latent_dim=LATENT_DIM, output_dim=END_EFFECTOR_DIM)
    aux_losses.append((GAMMA_BIN_PRED, aux))

if USE_FLOW_PRIOR:
    lm = BaseFlowVAE(
        input_dim=GENE_DIM,
        latent_dim=LATENT_DIM,
        hidden_dims=HIDDEN_DIMS,
        beta=BETA,
        num_flows=NUM_FLOWS,
        flow_hidden=FLOW_HIDDEN,
        flow_hidden_layers=FLOW_HIDDEN_LAYERS,
        aux_losses=aux_losses if aux_losses else None,
    )
    translate_fn = lm.translate
else:
    lm = BaseBetaVAE(
        input_dim=GENE_DIM,
        latent_dim=LATENT_DIM,
        hidden_dims=HIDDEN_DIMS,
        beta=BETA,
        aux_losses=aux_losses if aux_losses else None,
    )

if USE_STD_SUPPORT_LVE:
    operators.append(StandardNormalSupportLVE(lo=STD_SUPPORT_LO, hi=STD_SUPPORT_HI, translate_fn=translate_fn))

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

sslve = SSLVE(
    search_phase=sp,
    collector=collector,
    behavior_matching=bm,
    latent_module=lm,
    device=DEVICE,
)

# =============================================================================
# Run
# =============================================================================
print(f"N joints: {N_JOINTS}")
print(f"End-effector dim: {END_EFFECTOR_DIM}")
print(f"Angles per joint: {ANGLES_PER_JOINT}")
print(f"Gene dim: {GENE_DIM}")
print(f"Noise sigma: {NOISE_SIGMA}, Episodes: {N_NOISE_EPISODES}")
print(f"Fitness: {FITNESS}")
print(f"Latent dim: {LATENT_DIM}, Hidden: {HIDDEN_DIMS}")
if USE_FLOW_PRIOR:
    print(f"Flow prior: {NUM_FLOWS} flows, hidden: {FLOW_HIDDEN}x{FLOW_HIDDEN_LAYERS}")
print(f"Device: {DEVICE}")
print(f"N bins: {N_BINS} ({CENTERS})")
print(f"Samples per step: {N_TOTAL}")
print(f"Warmup threshold: {WARMUP_THRESHOLD}")
print(f"Operators: {[op.name for op in operators]}")
print(f"Warmup operators: {[op.name for op in warmup_operators]}")
if USE_BIN_PRED:
    print(f"BinPred gamma: {GAMMA_BIN_PRED}")
print()

train_kwargs = {
    'epochs': EPOCHS,
    'batch_size': BATCH_SIZE,
    'lr': LR,
    'verbose': True,
}

histories = sslve.run(n_steps=N_STEPS, train_kwargs=train_kwargs)

# =============================================================================
# Results
# =============================================================================
f_min, f_mean, f_max = bm.fitness_stats()
print(f"\nFinal archive size: {bm.archive_size()}")
print(f"Final coverage: {bm.coverage():.4f}")
print(f"Best fitness ({FITNESS}): {f_min:.4f}")

# =============================================================================
# Plot
# =============================================================================
sslve.plot_history()
sp.plot_allocation()
