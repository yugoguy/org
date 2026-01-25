#@title Example

import numpy as np
import random
import torch
from tqdm.auto import tqdm
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

# BetaTCVAE Loss Weights (alpha=beta=gamma=1 -> standard VAE)
ALPHA = 1.0  #@param {type:"number"}
BETA = 1.0  #@param {type:"number"}
GAMMA = 1.0  #@param {type:"number"}

# LVE GA
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
if hasattr(creator, "FitnessMax"):
    del creator.FitnessMax
if hasattr(creator, "Individual"):
    del creator.Individual

creator.create("FitnessMax", base.Fitness, weights=(1.0,))
creator.create("Individual", list, fitness=creator.FitnessMax)

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
    
    # Evaluate using negative |constraint| as fitness (maximize = minimize |constraint|)
    for ind in pop:
        constraint_val = problem.constraint(ind)
        ind.fitness.values = (-abs(constraint_val),)
    
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
                ind.fitness.values = (-abs(constraint_val),)
        
        pop[:] = offspring
        
        # Early stop if constraint satisfied (fitness == 0)
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
def calculate_values(ind, lve):
    """Calculate objective and constraint for a decoded individual."""
    problem = lve.data_generation.problem
    decoded = lve.decode([ind])[0]
    return {
        'obj': problem.fitness(decoded),
        'constraint': problem.constraint(decoded)
    }

def play(ind0, ind1):
    """Tournament match: compare objective and constraint."""
    # Objective comparison (minimization)
    if ind0.calculated_values['obj'] == ind1.calculated_values['obj']:
        ind0.gathered_score += 1
        ind1.gathered_score += 1
    elif ind0.calculated_values['obj'] < ind1.calculated_values['obj']:
        ind0.gathered_score += 1
    else:
        ind1.gathered_score += 1
    
    # Constraint comparison (0 = satisfied, want <= 0)
    c0 = ind0.calculated_values['constraint']
    c1 = ind1.calculated_values['constraint']
    
    c0_feasible = c0 <= 0
    c1_feasible = c1 <= 0
    
    if not c0_feasible and not c1_feasible:  # both infeasible
        if c0 < c1:
            ind0.gathered_score += 1
        else:
            ind1.gathered_score += 1
    else:
        if c0_feasible:
            ind0.gathered_score += 1
        if c1_feasible:
            ind1.gathered_score += 1
    
    ind0.num_matches += 1
    ind1.num_matches += 1

def evolve_fn(toolbox, lve, pop_size, n_gen):
    """LVE loop in latent space with tournament-based constraint handling."""
    problem = lve.data_generation.problem
    
    # Initialize population using LVE method
    pop = lve.init_population(pop_size)
    
    # Calculate values and initialize scores
    for ind in pop:
        ind.calculated_values = calculate_values(ind, lve)
        ind.gathered_score = 0
        ind.num_matches = 0
    
    # Initial tournament
    for ind in pop:
        participants = random.sample(pop, min(5, len(pop)))
        for _ in range(10):
            duel = random.sample(participants, 2)
            play(duel[0], duel[1])
    
    # Set fitness (maximizing score)
    for ind in pop:
        if ind.num_matches == 0:
            ind.fitness.values = (0,)
        else:
            ind.fitness.values = (ind.gathered_score / ind.num_matches,)
    
    best_obj = float('inf')
    
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
        
        # Recalculate all values and reset scores
        for ind in offspring:
            ind.calculated_values = calculate_values(ind, lve)
            ind.gathered_score = 0
            ind.num_matches = 0
        
        # Tournament
        for ind in offspring:
            participants = random.sample(offspring, min(5, len(offspring)))
            for _ in range(10):
                duel = random.sample(participants, 2)
                play(duel[0], duel[1])
        
        # Set fitness
        for ind in offspring:
            if ind.num_matches == 0:
                ind.fitness.values = (0,)
            else:
                ind.fitness.values = (ind.gathered_score / ind.num_matches,)
        
        pop[:] = offspring
        
        # Track best feasible objective
        for ind in pop:
            if ind.calculated_values['constraint'] <= 0:
                if ind.calculated_values['obj'] < best_obj:
                    best_obj = ind.calculated_values['obj']
        
        if (gen + 1) % 10 == 0:
            print(f"LVE Gen {gen + 1}/{n_gen}, Best feasible obj: {best_obj:.4f}")
    
    # Return best feasible solution, or best overall if none feasible
    feasible = [ind for ind in pop if ind.calculated_values['constraint'] <= 0]
    if feasible:
        best_ind = min(feasible, key=lambda x: x.calculated_values['obj'])
    else:
        best_ind = min(pop, key=lambda x: x.calculated_values['constraint'])
    
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
EPOCHS = 100  #@param {type:"integer"}
BATCH_SIZE = 32  #@param {type:"integer"}
LR = 1e-3  #@param {type:"number"}
VAL_SPLIT = 0.2  #@param {type:"number"}
latent_module = BetaTCVAE(DIM, LATENT_DIM, HIDDEN_DIM, alpha=ALPHA, beta=BETA, gamma=GAMMA)
lve = LVE(data_gen, latent_module, toolbox_lve, device=DEVICE,
          init_from_dataset=INIT_FROM_DATASET, init_epsilon=INIT_EPSILON)
lve.dataset = dataset
loss_history = lve.train_module(epochs=EPOCHS, batch_size=BATCH_SIZE, lr=LR, val_split=VAL_SPLIT)

print("\n" + "=" * 50)
print("Step 3: Latent Variable Evolution")
print("=" * 50)
POP_SIZE_LVE = 50  #@param {type:"integer"}
N_GEN_LVE = 30  #@param {type:"integer"}
best_solution = lve.evolve(POP_SIZE_LVE, N_GEN_LVE, evolve_fn)

print("\n" + "=" * 50)
print("Results")
print("=" * 50)
print(f"Best solution fitness: {problem.fitness(best_solution):.4f}")
print(f"Best solution constraint: {problem.constraint(best_solution):.4f}")
print(f"Feasible: {problem.is_feasible(best_solution)}")
