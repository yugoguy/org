#@title Example

import numpy as np
import random
import torch
from tqdm import tqdm
from deap import base, creator, tools

# =============================================================================
# Hyperparameters
# =============================================================================
# Problem
DIM = 10  #@param {type:"integer"}
X_MIN = -50.0  #@param {type:"number"}
X_MAX = 50.0  #@param {type:"number"}

# Data Generation GA
NUM_DATA_POINTS = 1000  #@param {type:"integer"}
POP_SIZE_DATA = 100  #@param {type:"integer"}
N_GEN_DATA = 50  #@param {type:"integer"}
CXPB_DATA = 0.5  #@param {type:"number"}
MUTPB_DATA = 0.2  #@param {type:"number"}
TOURNSIZE_DATA = 3  #@param {type:"integer"}
INDPB_DATA = 0.2  #@param {type:"number"}

# Latent Module
LATENT_DIM = 5  #@param {type:"integer"}
HIDDEN_DIM = 32  #@param {type:"integer"}
EPOCHS = 100  #@param {type:"integer"}
BATCH_SIZE = 32  #@param {type:"integer"}
LR = 1e-3  #@param {type:"number"}

# BetaTCVAE Loss Weights (alpha=beta=gamma=1 -> standard VAE)
ALPHA = 1.0  #@param {type:"number"}
BETA = 1.0  #@param {type:"number"}
GAMMA = 1.0  #@param {type:"number"}

# LVE GA
POP_SIZE_LVE = 50  #@param {type:"integer"}
N_GEN_LVE = 30  #@param {type:"integer"}
CXPB_LVE = 0.7  #@param {type:"number"}
MUTPB_LVE = 0.3  #@param {type:"number"}
TOURNSIZE_LVE = 3  #@param {type:"integer"}
INDPB_LVE = 0.2  #@param {type:"number"}

# LVE Initialization
INIT_FROM_DATASET = False  #@param {type:"boolean"}
INIT_EPSILON = 0.0  #@param {type:"number"}

# General
SEED = 42  #@param {type:"integer"}
DEVICE = 'cuda' if torch.cuda.is_available() else 'cpu'  #@param {type:"string"}

random.seed(SEED)
np.random.seed(SEED)
torch.manual_seed(SEED)

# =============================================================================
# DEAP Toolbox Setup for Data Generation
# =============================================================================
if hasattr(creator, "FitnessMin"):
    del creator.FitnessMin
if hasattr(creator, "Individual"):
    del creator.Individual

creator.create("FitnessMin", base.Fitness, weights=(-1.0,))
creator.create("Individual", list, fitness=creator.FitnessMin)

toolbox_data = base.Toolbox()
toolbox_data.register("attr_float", random.uniform, X_MIN, X_MAX)
toolbox_data.register("individual", tools.initRepeat, creator.Individual, toolbox_data.attr_float, n=DIM)
toolbox_data.register("population", tools.initRepeat, list, toolbox_data.individual)
toolbox_data.register("select", tools.selTournament, tournsize=TOURNSIZE_DATA)
toolbox_data.register("mate", tools.cxBlend, alpha=0.5)
toolbox_data.register("mutate", tools.mutGaussian, mu=0, sigma=10, indpb=INDPB_DATA)

# =============================================================================
# Generate Function for Data Generation
# =============================================================================
def single_ga_run(toolbox, problem):
    """Single GA run minimizing constraint. Returns best individual if feasible, else None."""
    pop = toolbox.population(n=POP_SIZE_DATA)
    
    # Evaluate using constraint as fitness
    for ind in pop:
        constraint_val = problem.constraint(ind)
        ind.fitness.values = (abs(constraint_val),)  # minimize |constraint|
    
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
            for i in range(len(ind)):
                ind[i] = np.clip(ind[i], X_MIN, X_MAX)
        
        for ind in offspring:
            if not ind.fitness.valid:
                constraint_val = problem.constraint(ind)
                ind.fitness.values = (abs(constraint_val),)
        
        pop[:] = offspring
        
        # Early stop if constraint satisfied
        best_ind = tools.selBest(pop, 1)[0]
        if best_ind.fitness.values[0] == 0.0:
            break
    
    best_ind = tools.selBest(pop, 1)[0]
    if problem.is_feasible(best_ind):
        return best_ind
    return None

def generate_fn(toolbox, problem):
    """Repeatedly run GA until NUM_DATA_POINTS feasible solutions collected."""
    dataset = []
    pbar = tqdm(total=NUM_DATA_POINTS, desc="Generating data")
    
    while len(dataset) < NUM_DATA_POINTS:
        valid_data = single_ga_run(toolbox, problem)
        if valid_data is not None:
            dataset.append(np.array(valid_data))
            pbar.update(1)
    
    pbar.close()
    return dataset

# =============================================================================
# DEAP Toolbox Setup for LVE
# =============================================================================
toolbox_lve = base.Toolbox()
toolbox_lve.register("attr_latent", random.uniform, -2, 2)
toolbox_lve.register("individual", tools.initRepeat, creator.Individual, toolbox_lve.attr_latent, n=LATENT_DIM)
toolbox_lve.register("population", tools.initRepeat, list, toolbox_lve.individual)
toolbox_lve.register("select", tools.selTournament, tournsize=TOURNSIZE_LVE)
toolbox_lve.register("mate", tools.cxBlend, alpha=0.5)
toolbox_lve.register("mutate", tools.mutGaussian, mu=0, sigma=0.5, indpb=INDPB_LVE)

# =============================================================================
# Evolve Function for LVE
# =============================================================================
def evolve_fn(toolbox, lve, pop_size, n_gen):
    """LVE loop in latent space."""
    problem = lve.data_generation.problem
    
    # Initialize population using LVE method
    pop = lve.init_population(pop_size)
    
    # Evaluate initial population
    decoded = lve.decode(pop)
    for i, ind in enumerate(pop):
        fit, _ = problem.evaluate(decoded[i])
        ind.fitness.values = (fit,)
    
    best_fitness = float('inf')
    
    for gen in range(n_gen):
        offspring = toolbox.select(pop, len(pop))
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
        
        # Evaluate offspring
        invalid_ind = [ind for ind in offspring if not ind.fitness.valid]
        if len(invalid_ind) > 0:
            decoded = lve.decode(invalid_ind)
            for i, ind in enumerate(invalid_ind):
                fit, _ = problem.evaluate(decoded[i])
                ind.fitness.values = (fit,)
        
        pop[:] = offspring
        
        current_best = min(ind.fitness.values[0] for ind in pop)
        if current_best < best_fitness:
            best_fitness = current_best
        
        if (gen + 1) % 10 == 0:
            print(f"LVE Gen {gen + 1}/{n_gen}, Best: {best_fitness:.4f}")
    
    # Return best solution in original space
    best_ind = min(pop, key=lambda x: x.fitness.values[0])
    return lve.decode([best_ind])[0]

# =============================================================================
# Full Run
# =============================================================================
print("=" * 50)
print("Step 1: Data Generation")
print("=" * 50)
problem = SphereProblem(dim=DIM, constraint_handling='rejection')
data_gen = DataGeneration(problem, toolbox_data, generate_fn)
dataset = data_gen.generate()
print(f"Dataset size: {len(dataset)}")

print("\n" + "=" * 50)
print("Step 2: Train Latent Module (BetaTCVAE)")
print("=" * 50)
latent_module = BetaTCVAE(DIM, LATENT_DIM, HIDDEN_DIM, alpha=ALPHA, beta=BETA, gamma=GAMMA)
lve = LVE(data_gen, latent_module, toolbox_lve, device=DEVICE,
          init_from_dataset=INIT_FROM_DATASET, init_epsilon=INIT_EPSILON)
lve.dataset = dataset
losses = lve.train_module(epochs=EPOCHS, batch_size=BATCH_SIZE, lr=LR)

print("\n" + "=" * 50)
print("Step 3: Latent Variable Evolution")
print("=" * 50)
best_solution = lve.evolve(POP_SIZE_LVE, N_GEN_LVE, evolve_fn)

print("\n" + "=" * 50)
print("Results")
print("=" * 50)
print(f"Best solution fitness: {problem.fitness(best_solution):.4f}")
print(f"Best solution constraint: {problem.constraint(best_solution):.4f}")
print(f"Feasible: {problem.is_feasible(best_solution)}")
