#@title TravelAntOmniGrid BoltzmannMix Any
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

# --- Variation Operators ---
USE_PSE_MUT = True  #@param {type:"boolean"}
USE_PSE_LINE = True  #@param {type:"boolean"}
USE_LVE_MUT = False  #@param {type:"boolean"}
USE_LVE_CROSS = False  #@param {type:"boolean"}
USE_STD_SUPPORT_LVE = False  #@param {type:"boolean"}
GREEDY_MEM = True  #@param {type:"boolean"}

PSE_MUT_SIGMA = 0.01  #@param {type:"number"}
LVE_MUT_SIGMA = 0.05  #@param {type:"number"}
STD_SUPPORT_LO = -2.0  #@param {type:"number"}
STD_SUPPORT_HI = 2.0  #@param {type:"number"}

# --- Warmup Operators ---
WARMUP_PSE_MUT = True  #@param {type:"boolean"}
WARMUP_PSE_LINE = True  #@param {type:"boolean"}

N_TOTAL = 500  #@param {type:"integer"}
WARMUP_THRESHOLD = 512  #@param {type:"integer"}
EMA_ALPHA = 0.3  #@param {type:"number"}
TEMPERATURE = 0.5  #@param {type:"number"}
MIN_PROPORTION = 0.05  #@param {type:"number"}

# --- Latent Module ---
USE_FLOW_PRIOR = False  #@param {type:"boolean"}
LATENT_DIM = 128  #@param {type:"integer"}
HIDDEN_DIMS = [512, 256]  #@param
BETA = 1e-2  #@param {type:"number"}
NUM_FLOWS = 3  #@param {type:"integer"}
FLOW_HIDDEN = 256  #@param {type:"integer"}
FLOW_HIDDEN_LAYERS = 2  #@param {type:"integer"}
EPOCHS = 10  #@param {type:"integer"}
BATCH_SIZE = 512  #@param {type:"integer"}
LR = 1e-3  #@param {type:"number"}

# --- Aux Losses ---
USE_BIN_PRED = False  #@param {type:"boolean"}
GAMMA_BIN_PRED = 1e-3  #@param {type:"number"}
USE_MIX_BIN_PRED = False  #@param {type:"boolean"}
GAMMA_MIX_BIN_PRED = 1e-3  #@param {type:"number"}
MIX_ALPHA_LO = -0.1  #@param {type:"number"}
MIX_ALPHA_HI = 1.1  #@param {type:"number"}

N_STEPS = 5000  #@param {type:"integer"}

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

# --- Determine if any LVE operator is used ---
USE_LVE = USE_LVE_MUT or USE_LVE_CROSS or USE_STD_SUPPORT_LVE

# --- Build operators ---
operators = []
translate_fn = None
if USE_PSE_MUT:
    operators.append(PSEMut(sigma=PSE_MUT_SIGMA, greedy_mem=GREEDY_MEM))
if USE_PSE_LINE:
    operators.append(PSELine(greedy_mem=GREEDY_MEM))
if USE_LVE_MUT:
    operators.append(LVEMut(sigma=LVE_MUT_SIGMA, greedy_mem=GREEDY_MEM))
if USE_LVE_CROSS:
    operators.append(LVECross(greedy_mem=GREEDY_MEM))
assert len(operators) > 0, "At least one variation operator must be enabled."

# --- Build warmup operators ---
warmup_operators = []
if WARMUP_PSE_MUT:
    warmup_operators.append(PSEMut(sigma=PSE_MUT_SIGMA, greedy_mem=GREEDY_MEM))
if WARMUP_PSE_LINE:
    warmup_operators.append(PSELine(greedy_mem=GREEDY_MEM))
assert len(warmup_operators) > 0, "At least one warmup operator must be enabled."

# --- Build latent module and aux losses (only if LVE used) ---
lm = None
if USE_LVE:
    aux_losses = []
    if USE_BIN_PRED:
        aux = BinPred(behavior_descriptor=bd, latent_dim=LATENT_DIM, output_dim=2)
        aux_losses.append((GAMMA_BIN_PRED, aux))

    if USE_MIX_BIN_PRED:
        mix_aux = MixBinPred(
            behavior_descriptor=bd, latent_dim=LATENT_DIM, output_dim=2,
            alpha_lo=MIX_ALPHA_LO, alpha_hi=MIX_ALPHA_HI,
        )
        aux_losses.append((GAMMA_MIX_BIN_PRED, mix_aux))

    if USE_FLOW_PRIOR:
        lm = BaseFlowVAE(
            input_dim=weight_dim,
            latent_dim=LATENT_DIM,
            hidden_dims=HIDDEN_DIMS,
            beta=BETA,
            num_flows=NUM_FLOWS,
            flow_hidden=FLOW_HIDDEN,
            flow_hidden_layers=FLOW_HIDDEN_LAYERS,
            aux_losses=aux_losses,
        )
        translate_fn = lm.translate
        if USE_MIX_BIN_PRED:
            mix_aux.to_base_fn = lambda z: lm.flow.f(z)[0]
            mix_aux.from_base_fn = lm.flow.f_inv
    else:
        lm = BaseBetaVAE(
            input_dim=weight_dim,
            latent_dim=LATENT_DIM,
            hidden_dims=HIDDEN_DIMS,
            beta=BETA,
            aux_losses=aux_losses,
        )

    if USE_STD_SUPPORT_LVE:
        operators.append(StandardNormalSupportLVE(lo=STD_SUPPORT_LO, hi=STD_SUPPORT_HI, translate_fn=translate_fn))

sp = BoltzmannMix(
    agent_class=MLP_Agent,
    architecture=ARCHITECTURE,
    agent_kwargs={'output_activation': OUTPUT_ACTIVATION},
    operators=operators,
    warmup_operators=warmup_operators,
    n_total=N_TOTAL,
    warmup_threshold=WARMUP_THRESHOLD,
    ema_alpha=EMA_ALPHA,
    temperature=TEMPERATURE,
    min_proportion=MIN_PROPORTION,
)

# --- Build orchestrator ---
if USE_LVE:
    orchestrator = SSLVE(
        search_phase=sp,
        collector=collector,
        behavior_matching=bm,
        latent_module=lm,
        device=DEVICE,
    )
else:
    orchestrator = MAPElite(
        search_phase=sp,
        collector=collector,
        behavior_matching=bm,
    )

# --- Load checkpoint if specified ---
if CHECKPOINT_PATH:
    load_checkpoint(CHECKPOINT_PATH, bm, orchestrator.history, sp=sp, lm=lm, device=DEVICE)
    print(f"Loaded checkpoint from {CHECKPOINT_PATH}")
    print(f"Archive size: {bm.archive_size()}, Coverage: {bm.coverage():.4f}")

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
if USE_LVE:
    print(f"Latent dim: {LATENT_DIM}, Hidden: {HIDDEN_DIMS}")
    if USE_FLOW_PRIOR:
        print(f"Flow prior: {NUM_FLOWS} flows, hidden: {FLOW_HIDDEN}x{FLOW_HIDDEN_LAYERS}")
print(f"Device: {DEVICE}")
print(f"Samples per step: {N_TOTAL}")
print(f"Warmup threshold: {WARMUP_THRESHOLD}")
print(f"Operators: {[op.name for op in operators]}")
print(f"Warmup operators: {[op.name for op in warmup_operators]}")
if USE_BIN_PRED:
    print(f"BinPred gamma: {GAMMA_BIN_PRED}")
if USE_MIX_BIN_PRED:
    print(f"MixBinPred gamma: {GAMMA_MIX_BIN_PRED}, alpha: [{MIX_ALPHA_LO}, {MIX_ALPHA_HI}]")
print(f"Mode: {'SSLVE' if USE_LVE else 'MAPElite'}")
print()

if USE_LVE:
    train_kwargs = {
        'epochs': EPOCHS,
        'batch_size': BATCH_SIZE,
        'lr': LR,
        'verbose': True,
    }
    histories = orchestrator.run(n_steps=N_STEPS, train_kwargs=train_kwargs)
else:
    orchestrator.run(n_steps=N_STEPS)

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
orchestrator.plot_history()
sp.plot_allocation()
