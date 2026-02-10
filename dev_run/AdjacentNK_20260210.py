#@title Example

import numpy as np
import random
import torch
from tqdm.auto import tqdm
from deap import base, creator, tools

# =============================================================================
# Hyperparameters
# =============================================================================
# NK Landscape
N = 20
K = 5
ADJACENT = True
NK_SEED = 42

# Data Generation
NUM_DATA_POINTS = 1000
POP_SIZE_DATA = 100
N_GEN_DATA = 50
CXPB_DATA = 0.5
MUTPB_DATA = 0.2
TOURNSIZE_DATA = 3
FLIPBIT_INDPB = 0.1
GROUP_SIZE = 20
TOP_K_PER_GROUP = 5

# Latent Module
LATENT_DIM = 10
HIDDEN_DIM = 64
EPOCHS = 100
BATCH_SIZE = 128
LR = 1e-3
VAL_SPLIT = 0.2
BETA_VAE = 1.0

# LVE GA
POP_SIZE_LVE = 100
N_GEN_LVE = 200
CXPB_LVE = 0.7
MUTPB_LVE = 0.3
TOURNSIZE_LVE = 3
INDPB_LVE = 0.2
ELITE_SIZE = 5

# LVE Initialization
INIT_FROM_DATASET = False
INIT_EPSILON = 0.0

# General
SEED = 42
DEVICE = 'cuda' if torch.cuda.is_available() else 'cpu'

random.seed(SEED)
np.random.seed(SEED)
torch.manual_seed(SEED)

# =============================================================================
# Problem
# =============================================================================
problem = NKLandscape(n=N, k=K, adjacent=ADJACENT, seed=NK_SEED)

# =============================================================================
# DEAP Setup for Data Generation (binary)
# =============================================================================
if hasattr(creator, "FitnessMax"):
    del creator.FitnessMax
if hasattr(creator, "Individual"):
    del creator.Individual

creator.create("FitnessMax", base.Fitness, weights=(1.0,))
creator.create("Individual", list, fitness=creator.FitnessMax)

toolbox_data = base.Toolbox()
toolbox_data.register("attr_bit", random.randint, 0, 1)
toolbox_data.register("individual", tools.initRepeat, creator.Individual, toolbox_data.attr_bit, n=N)
toolbox_data.register("population", tools.initRepeat, list, toolbox_data.individual)
toolbox_data.register("select", tools.selTournament, tournsize=TOURNSIZE_DATA)
toolbox_data.register("mate", tools.cxUniform, indpb=0.5)
toolbox_data.register("mutate", tools.mutFlipBit, indpb=FLIPBIT_INDPB)

# =============================================================================
# Data Generation Function (diversified grouping)
# =============================================================================
def generate_fn(toolbox, problem):
    """
    Generate diverse dataset via grouping-based selection.
    Run a GA, then repeatedly group population randomly and select top-k per group.
    """
    pop = toolbox.population(n=POP_SIZE_DATA)
    
    # Evaluate initial population
    for ind in pop:
        ind.fitness.values = (-problem.fitness(ind),)  # negate: problem minimizes, DEAP maximizes
    
    # Run GA to get a decent population
    for gen in range(N_GEN_DATA):
        offspring = toolbox.select(pop, len(pop))
        offspring = list(map(toolbox.clone, offspring))
        
        for i in range(0, len(offspring) - 1, 2):
            if random.random() < CXPB_DATA:
                toolbox.mate(offspring[i], offspring[i + 1])
                del offspring[i].fitness.values
                del offspring[i + 1].fitness.values
        
        for mutant in offspring:
            if random.random() < MUTPB_DATA:
                toolbox.mutate(mutant)
                del mutant.fitness.values
        
        for ind in offspring:
            if not ind.fitness.valid:
                ind.fitness.values = (-problem.fitness(ind),)
        
        pop[:] = offspring
    
    # Collect dataset via grouping
    dataset = []
    pbar = tqdm(total=NUM_DATA_POINTS, desc="Generating data")
    
    while len(dataset) < NUM_DATA_POINTS:
        # Shuffle and group
        random.shuffle(pop)
        for g_start in range(0, len(pop), GROUP_SIZE):
            if len(dataset) >= NUM_DATA_POINTS:
                break
            group = pop[g_start:g_start + GROUP_SIZE]
            if len(group) < TOP_K_PER_GROUP:
                continue
            # Select top-k by fitness
            top = tools.selBest(group, TOP_K_PER_GROUP)
            for ind in top:
                if len(dataset) >= NUM_DATA_POINTS:
                    break
                dataset.append(np.array(ind, dtype=np.float32))
                pbar.update(1)
        
        # Evolve population further for more diversity
        offspring = toolbox.select(pop, len(pop))
        offspring = list(map(toolbox.clone, offspring))
        for i in range(0, len(offspring) - 1, 2):
            if random.random() < CXPB_DATA:
                toolbox.mate(offspring[i], offspring[i + 1])
                del offspring[i].fitness.values
                del offspring[i + 1].fitness.values
        for mutant in offspring:
            if random.random() < MUTPB_DATA:
                toolbox.mutate(mutant)
                del mutant.fitness.values
        for ind in offspring:
            if not ind.fitness.valid:
                ind.fitness.values = (-problem.fitness(ind),)
        pop[:] = offspring
    
    pbar.close()
    return dataset

# =============================================================================
# DEAP Setup for LVE (continuous latent space)
# =============================================================================
toolbox_lve = base.Toolbox()
toolbox_lve.register("attr_latent", random.uniform, -2, 2)
toolbox_lve.register("individual", tools.initRepeat, creator.Individual, toolbox_lve.attr_latent, n=LATENT_DIM)
toolbox_lve.register("population", tools.initRepeat, list, toolbox_lve.individual)
toolbox_lve.register("select", tools.selTournament, tournsize=TOURNSIZE_LVE)
toolbox_lve.register("mate", tools.cxBlend, alpha=0.5)
toolbox_lve.register("mutate", tools.mutGaussian, mu=0, sigma=0.5, indpb=INDPB_LVE)

# =============================================================================
# LVE Evolve Function
# =============================================================================
def evaluate_individual(ind, lve):
    """Evaluate individual. Returns (fitness,) for minimization problem."""
    problem = lve.data_generation.problem
    decoded = lve.decode([ind])[0]
    # Binarize decoded output
    binary = (decoded >= 0.5).astype(int).tolist()
    raw_fit = problem.fitness(binary)
    return raw_fit, raw_fit, 0.0


def record_generation_stats(pop, lve, history):
    problem = lve.data_generation.problem
    raw_fitnesses = []
    for ind in pop:
        decoded = lve.decode([ind])[0]
        binary = (decoded >= 0.5).astype(int).tolist()
        raw_fitnesses.append(problem.fitness(binary))
    raw_fitnesses = np.array(raw_fitnesses)
    history['fitness']['mean'].append(float(raw_fitnesses.mean()))
    history['fitness']['min'].append(float(raw_fitnesses.min()))
    history['fitness']['max'].append(float(raw_fitnesses.max()))
    history['fitness']['var'].append(float(raw_fitnesses.var()))


def evolve_fn(toolbox, lve, pop_size, n_gen):
    """LVE evolution in latent space."""
    history = {'fitness': {'mean': [], 'min': [], 'max': [], 'var': []}}
    
    pop = lve.init_population(pop_size)
    
    for ind in pop:
        raw_fit, _, _ = evaluate_individual(ind, lve)
        ind.fitness.values = (-raw_fit,)  # negate: minimize fitness, DEAP maximizes
    
    record_generation_stats(pop, lve, history)
    
    for gen in range(n_gen):
        elite = tools.selBest(pop, ELITE_SIZE)
        elite = list(map(toolbox.clone, elite))
        
        offspring = toolbox.select(pop, len(pop) - ELITE_SIZE)
        offspring = list(map(toolbox.clone, offspring))
        
        for i in range(0, len(offspring) - 1, 2):
            if random.random() < CXPB_LVE:
                toolbox.mate(offspring[i], offspring[i + 1])
                del offspring[i].fitness.values
                del offspring[i + 1].fitness.values
        
        for mutant in offspring:
            if random.random() < MUTPB_LVE:
                toolbox.mutate(mutant)
                del mutant.fitness.values
        
        for ind in offspring:
            if not ind.fitness.valid:
                raw_fit, _, _ = evaluate_individual(ind, lve)
                ind.fitness.values = (-raw_fit,)
        
        pop[:] = elite + offspring
        record_generation_stats(pop, lve, history)
        
        if (gen + 1) % 10 == 0:
            print(f"LVE Gen {gen+1}/{n_gen}, Fitness mean: {history['fitness']['mean'][-1]:.4f}, "
                  f"min: {history['fitness']['min'][-1]:.4f}")
    
    lve.evolution_history = history
    best_ind = max(pop, key=lambda x: x.fitness.values[0])
    decoded = lve.decode([best_ind])[0]
    return (decoded >= 0.5).astype(int)

# =============================================================================
# Run
# =============================================================================
print("Step 1: Data Generation")
data_gen = DataGeneration(problem, toolbox_data, generate_fn)
dataset = data_gen.generate()
print(f"Dataset size: {len(dataset)}")

print("\nStep 2: Train BinaryBetaVAE")
latent_module = BinaryBetaVAE(N, LATENT_DIM, HIDDEN_DIM, beta=BETA_VAE)
lve = LVE(data_gen, latent_module, toolbox_lve, device=DEVICE,
          init_from_dataset=INIT_FROM_DATASET, init_epsilon=INIT_EPSILON)
lve.dataset = dataset
loss_history = lve.train_module(epochs=EPOCHS, batch_size=BATCH_SIZE, lr=LR, val_split=VAL_SPLIT)

print("\nStep 3: Latent Variable Evolution")
best_solution = lve.evolve(POP_SIZE_LVE, N_GEN_LVE, evolve_fn)

print("\nResults")
print(f"Best solution: {best_solution}")
print(f"Best fitness (NK, higher=better): {-problem.fitness(best_solution):.4f}")

lve.plot_evolution()
