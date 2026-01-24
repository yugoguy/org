import numpy as np
import random
import torch
import torch.nn as nn
import torch.nn.functional as F
from deap import base, creator, tools

# =============================================================================
# Hyperparameters
# =============================================================================
# Problem
DIM = 10  #@param {type:"integer"}
X_MIN = -50.0  #@param {type:"number"}
X_MAX = 50.0  #@param {type:"number"}

# Data Generation GA
POP_SIZE_DATA = 100  #@param {type:"integer"}
N_GEN_DATA = 50  #@param {type:"integer"}
CXPB_DATA = 0.7  #@param {type:"number"}
MUTPB_DATA = 0.2  #@param {type:"number"}
TOURNSIZE_DATA = 3  #@param {type:"integer"}
INDPB_DATA = 0.1  #@param {type:"number"}

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

# General
SEED = 42  #@param {type:"integer"}
DEVICE = 'cuda' if torch.cuda.is_available() else 'cpu'  #@param {type:"string"}

random.seed(SEED)
np.random.seed(SEED)
torch.manual_seed(SEED)

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
# Concrete Latent Module (BetaTCVAE)
# =============================================================================
class BetaTCVAE(LatentModule):
    """
    Beta-TC VAE with decomposed KL divergence.
    Loss = Recon + alpha*MI + beta*TC + gamma*KL
    Setting alpha=beta=gamma=1 gives standard VAE.
    """
    
    def __init__(self, input_dim, latent_dim, hidden_dim=32, alpha=1.0, beta=1.0, gamma=1.0):
        super().__init__(input_dim, latent_dim)
        self.alpha = alpha
        self.beta = beta
        self.gamma = gamma
        
        # Encoder outputs mu and logvar
        self.encoder_net = nn.Sequential(
            nn.Linear(input_dim, hidden_dim),
            nn.ReLU(),
        )
        self.fc_mu = nn.Linear(hidden_dim, latent_dim)
        self.fc_logvar = nn.Linear(hidden_dim, latent_dim)
        
        # Decoder
        self.decoder_net = nn.Sequential(
            nn.Linear(latent_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, input_dim)
        )
    
    def encode(self, x):
        """Returns mu (for inference/evolution use)."""
        h = self.encoder_net(x)
        return self.fc_mu(h)
    
    def encode_dist(self, x):
        """Returns mu and logvar for training."""
        h = self.encoder_net(x)
        return self.fc_mu(h), self.fc_logvar(h)
    
    def reparameterize(self, mu, logvar):
        """Reparameterization trick."""
        std = torch.exp(0.5 * logvar)
        eps = torch.randn_like(std)
        return mu + eps * std
    
    def decode(self, z):
        return self.decoder_net(z)
    
    def forward(self, x):
        mu, logvar = self.encode_dist(x)
        z = self.reparameterize(mu, logvar)
        x_recon = self.decode(z)
        return x_recon, mu, logvar, z
    
    def log_density_gaussian(self, x, mu, logvar):
        """Log density of Gaussian."""
        norm = -0.5 * (np.log(2 * np.pi) + logvar)
        log_density = norm - 0.5 * ((x - mu) ** 2 / torch.exp(logvar))
        return log_density
    
    def loss(self, x, x_recon, mu=None, logvar=None, z=None, **kwargs):
        """
        Compute BetaTCVAE loss with decomposed KL.
        Uses minibatch weighted sampling for TC estimation.
        """
        batch_size = x.size(0)
        
        # Reconstruction loss
        recon_loss = F.mse_loss(x_recon, x, reduction='sum') / batch_size
        
        # Log q(z|x) - log density under encoder
        log_qz_x = self.log_density_gaussian(z, mu, logvar).sum(dim=1)
        
        # Log p(z) - log density under prior N(0,1)
        zeros = torch.zeros_like(z)
        log_pz = self.log_density_gaussian(z, zeros, zeros).sum(dim=1)
        
        # Log q(z) - marginal (minibatch weighted sampling)
        # For each z_i, compute log q(z_i) = log (1/N) sum_j q(z_i|x_j)
        _logqz = self.log_density_gaussian(
            z.unsqueeze(1),  # (batch, 1, latent)
            mu.unsqueeze(0),  # (1, batch, latent)
            logvar.unsqueeze(0)  # (1, batch, latent)
        )  # (batch, batch, latent)
        
        # log q(z) - sum over latent dims, logsumexp over batch
        log_qz = torch.logsumexp(_logqz.sum(dim=2), dim=1) - np.log(batch_size)
        
        # log prod_j q(z_j) - product of marginals
        log_qz_product = (torch.logsumexp(_logqz, dim=1) - np.log(batch_size)).sum(dim=1)
        
        # Decomposed KL terms
        # MI: E[log q(z|x) - log q(z)]
        mi_loss = (log_qz_x - log_qz).mean()
        
        # TC: E[log q(z) - log prod_j q(z_j)]
        tc_loss = (log_qz - log_qz_product).mean()
        
        # Dimension-wise KL: E[log prod_j q(z_j) - log p(z)]
        kl_loss = (log_qz_product - log_pz).mean()
        
        total_loss = recon_loss + self.alpha * mi_loss + self.beta * tc_loss + self.gamma * kl_loss
        
        return total_loss
    
    def fit(self, dataset, epochs=100, batch_size=32, lr=1e-3, device='cpu', verbose=True):
        """Training loop for VAE."""
        self.to(device)
        self.train()
        
        data = torch.tensor(np.array(dataset), dtype=torch.float32)
        data_loader = torch.utils.data.DataLoader(data, batch_size=batch_size, shuffle=True, drop_last=True)
        
        optimizer = torch.optim.Adam(self.parameters(), lr=lr)
        losses = []
        
        for epoch in range(epochs):
            epoch_loss = 0.0
            for batch in data_loader:
                batch = batch.to(device)
                optimizer.zero_grad()
                
                x_recon, mu, logvar, z = self.forward(batch)
                loss = self.loss(batch, x_recon, mu=mu, logvar=logvar, z=z)
                
                loss.backward()
                optimizer.step()
                epoch_loss += loss.item()
            
            avg_loss = epoch_loss / len(data_loader)
            losses.append(avg_loss)
            
            if verbose and (epoch + 1) % 10 == 0:
                print(f"Epoch {epoch + 1}/{epochs}, Loss: {avg_loss:.6f}")
        
        self.eval()
        return losses

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
def generate_fn(toolbox, problem):
    """GA loop for dataset generation."""
    dataset = []
    pop = toolbox.population(n=POP_SIZE_DATA)
    
    for ind in pop:
        fit, feasible = problem.evaluate(ind)
        ind.fitness.values = (fit,)
        if feasible:
            dataset.append(np.array(ind))
    
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
                fit, feasible = problem.evaluate(ind)
                ind.fitness.values = (fit,)
                if feasible:
                    dataset.append(np.array(ind))
        
        if problem.constraint_handling == 'rejection':
            feasible_offspring = [ind for ind in offspring if problem.is_feasible(ind)]
            if len(feasible_offspring) >= 2:
                offspring = feasible_offspring
        
        pop[:] = offspring
    
    return dataset

# =============================================================================
# DEAP Toolbox Setup for LVE
# =============================================================================
toolbox_lve = base.Toolbox()
toolbox_lve.register("attr_latent", random.gauss, 0, 1)
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
    
    # Initialize population in latent space
    pop = toolbox.population(n=pop_size)
    
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
lve = LVE(data_gen, latent_module, toolbox_lve, device=DEVICE)
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
