#@title PlanarArmCVT CMA-ME
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

# --- CMA-ME ---
N_EMITTERS = 10  #@param {type:"integer"}
SIGMA_INIT = 0.5  #@param {type:"number"}
POPSIZE = 50  #@param {type:"integer"}
GREEDY_MEM = True  #@param {type:"boolean"}
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

orchestrator = CMAME(
    agent_class=PlanarArmAgent,
    architecture=GENE_DIM,
    collector=collector,
    behavior_matching=bm,
    n_emitters=N_EMITTERS,
    sigma_init=SIGMA_INIT,
    popsize=POPSIZE,
    greedy_mem=GREEDY_MEM,
    n_init_samples=N_INIT_SAMPLES,
    init_fn=init_fn,
)

# --- Load checkpoint if specified ---
if CHECKPOINT_PATH:
    load_cmame_checkpoint(CHECKPOINT_PATH, orchestrator)
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
print(f"N bins: {N_BINS} ({CENTERS})")
print(f"N emitters: {N_EMITTERS}, Popsize: {POPSIZE}, Sigma: {SIGMA_INIT}")
print(f"Evals per step: {N_EMITTERS * POPSIZE}")
print(f"Init samples: {N_INIT_SAMPLES}")
print(f"Mode: CMAME")
print()

orchestrator.run(n_steps=N_STEPS)

# =============================================================================
# Results
# =============================================================================
f_min, f_mean, f_max = bm.fitness_stats()
print(f"\nFinal archive size: {bm.archive_size()}")
print(f"Final coverage: {bm.coverage():.4f}")
print(f"Best fitness ({FITNESS}): {f_min:.4f}")
print(f"QD-score: {bm.qd_score():.4f}")
print(f"Total evaluations: {orchestrator.total_evals}")

# =============================================================================
# Plot
# =============================================================================
orchestrator.plot_history()
