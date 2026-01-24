import numpy as np
import random
from deap import base, creator, tools

# =============================================================================
# Hyperparameters
# =============================================================================
DIM = 10  #@param {type:"integer"}
POP_SIZE = 100  #@param {type:"integer"}
N_GEN = 50  #@param {type:"integer"}
CXPB = 0.7  #@param {type:"number"}
MUTPB = 0.2  #@param {type:"number"}
TOURNSIZE = 3  #@param {type:"integer"}
INDPB = 0.1  #@param {type:"number"}
X_MIN = -50.0  #@param {type:"number"}
X_MAX = 50.0  #@param {type:"number"}
SEED = 42  #@param {type:"integer"}

random.seed(SEED)
np.random.seed(SEED)

# =============================================================================
# Problem Definition (Sphere + C1)
# =============================================================================
class SphereProblem(ProblemSetting):
    """
    Sphere function with C1 constraint.
    fitness: f(x) = sum(x_i^2)
    constraint: sum(45 - x_i) <= 0  (i.e., sum(x_i) >= 45*D)
    """
    
    def __init__(self, dim, constraint_handling='rejection', penalty_coef=1e6):
        super().__init__(constraint_handling, penalty_coef)
        self.dim = dim
    
    def fitness(self, x):
        return sum(xi ** 2 for xi in x)
    
    def constraint(self, x):
        return sum(45 - xi for xi in x)

# =============================================================================
# DEAP Toolbox Setup
# =============================================================================
if hasattr(creator, "FitnessMin"):
    del creator.FitnessMin
if hasattr(creator, "Individual"):
    del creator.Individual

creator.create("FitnessMin", base.Fitness, weights=(-1.0,))
creator.create("Individual", list, fitness=creator.FitnessMin)

toolbox = base.Toolbox()
toolbox.register("attr_float", random.uniform, X_MIN, X_MAX)
toolbox.register("individual", tools.initRepeat, creator.Individual, toolbox.attr_float, n=DIM)
toolbox.register("population", tools.initRepeat, list, toolbox.individual)
toolbox.register("select", tools.selTournament, tournsize=TOURNSIZE)
toolbox.register("mate", tools.cxBlend, alpha=0.5)
toolbox.register("mutate", tools.mutGaussian, mu=0, sigma=10, indpb=INDPB)

# =============================================================================
# Generate Function (GA Loop)
# =============================================================================
def generate_fn(toolbox, problem):
    """
    GA loop for dataset generation.
    Returns list of feasible individuals (as numpy arrays).
    """
    dataset = []
    pop = toolbox.population(n=POP_SIZE)
    
    # Evaluate initial population
    for ind in pop:
        fit, feasible = problem.evaluate(ind)
        ind.fitness.values = (fit,)
        if feasible:
            dataset.append(np.array(ind))
    
    for gen in range(N_GEN):
        # Selection
        offspring = toolbox.select(pop, len(pop))
        offspring = list(map(toolbox.clone, offspring))
        
        # Crossover
        for i in range(0, len(offspring) - 1, 2):
            if random.random() < CXPB:
                toolbox.mate(offspring[i], offspring[i + 1])
                del offspring[i].fitness.values
                del offspring[i + 1].fitness.values
        
        # Mutation
        for mutant in offspring:
            if random.random() < MUTPB:
                toolbox.mutate(mutant)
                del mutant.fitness.values
        
        # Clip to bounds
        for ind in offspring:
            for i in range(len(ind)):
                ind[i] = np.clip(ind[i], X_MIN, X_MAX)
        
        # Evaluate
        for ind in offspring:
            if not ind.fitness.valid:
                fit, feasible = problem.evaluate(ind)
                ind.fitness.values = (fit,)
                if feasible:
                    dataset.append(np.array(ind))
        
        # Rejection filter: keep only feasible for next gen
        if problem.constraint_handling == 'rejection':
            feasible_offspring = [ind for ind in offspring if problem.is_feasible(ind)]
            if len(feasible_offspring) >= 2:
                offspring = feasible_offspring
        
        pop[:] = offspring
    
    return dataset

# =============================================================================
# Run
# =============================================================================
problem = SphereProblem(dim=DIM, constraint_handling='rejection')
data_gen = DataGeneration(problem, toolbox, generate_fn)
dataset = data_gen.generate()

print(f"Dataset size: {len(dataset)}")
if len(dataset) > 0:
    print(f"Sample shape: {dataset[0].shape}")
    print(f"Sample fitness: {problem.fitness(dataset[0]):.4f}")
    print(f"Sample constraint: {problem.constraint(dataset[0]):.4f}")
