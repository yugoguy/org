import numpy as np
import random
import torch

class LVE:
    """
    Latent Variable Evolution main class.
    
    Args:
        data_generation: DataGeneration instance
        latent_module: LatentModule instance
        toolbox: DEAP toolbox for latent space GA
        device: 'cpu' or 'cuda'
        init_from_dataset: if True, initialize latent pop from encoded dataset
        init_epsilon: noise scale added to mu (z = mu + epsilon * std)
    """
    
    def __init__(self, data_generation, latent_module, toolbox, device='cpu',
                 init_from_dataset=False, init_epsilon=0.0):
        self.data_generation = data_generation
        self.latent_module = latent_module
        self.toolbox = toolbox
        self.device = device
        self.init_from_dataset = init_from_dataset
        self.init_epsilon = init_epsilon
        self.dataset = None
    
    def regenerate(self):
        """Regenerate dataset using DataGeneration."""
        self.dataset = self.data_generation.generate()
        return self.dataset
    
    def train_module(self, dataset=None, **kwargs):
        """
        Train latent module.
        
        Args:
            dataset: optional dataset (if None, uses self.dataset)
            **kwargs: passed to latent_module.fit()
        
        Returns:
            list of losses per epoch
        """
        if dataset is None:
            if self.dataset is None:
                self.regenerate()
            dataset = self.dataset
        
        return self.latent_module.fit(dataset, device=self.device, **kwargs)
    
    def retrain(self, **kwargs):
        """Regenerate dataset and retrain module."""
        self.regenerate()
        return self.train_module(**kwargs)
    
    def encode(self, population):
        """
        Encode population to latent space.
        
        Args:
            population: list of individuals (lists or arrays)
        
        Returns:
            numpy array of latent vectors
        """
        self.latent_module.eval()
        with torch.no_grad():
            x = torch.tensor(np.array(population), dtype=torch.float32).to(self.device)
            z = self.latent_module.encode(x)
        return z.cpu().numpy()
    
    def decode(self, latent_population):
        """
        Decode latent population to original space.
        
        Args:
            latent_population: list of latent vectors (lists or arrays)
        
        Returns:
            numpy array of original space vectors
        """
        self.latent_module.eval()
        with torch.no_grad():
            z = torch.tensor(np.array(latent_population), dtype=torch.float32).to(self.device)
            x = self.latent_module.decode(z)
        return x.cpu().numpy()
    
    def _sample_latent_from_dataset(self):
        """Sample one latent vector from encoded dataset."""
        idx = random.randint(0, len(self.dataset) - 1)
        x = torch.tensor(self.dataset[idx], dtype=torch.float32).unsqueeze(0).to(self.device)
        self.latent_module.eval()
        with torch.no_grad():
            if hasattr(self.latent_module, 'encode_dist'):
                mu, logvar = self.latent_module.encode_dist(x)
                std = torch.exp(0.5 * logvar)
                z = mu + self.init_epsilon * std
            else:
                z = self.latent_module.encode(x)
        return z.squeeze(0).cpu().tolist()
    
    def init_population(self, pop_size):
        """
        Initialize latent population.
        
        Args:
            pop_size: population size
        
        Returns:
            list of DEAP individuals in latent space
        """
        from deap import tools
        
        if self.init_from_dataset and self.dataset is not None:
            pop = []
            for _ in range(pop_size):
                z = self._sample_latent_from_dataset()
                ind = self.toolbox.clone(self.toolbox.individual())
                for i in range(len(z)):
                    ind[i] = z[i]
                del ind.fitness.values
                pop.append(ind)
            return pop
        else:
            return self.toolbox.population(n=pop_size)
    
    def evolve(self, pop_size, n_gen, evolve_fn):
        """
        Main LVE loop.
        
        Args:
            pop_size: population size
            n_gen: number of generations
            evolve_fn: function(toolbox, lve, pop_size, n_gen) -> final population
                       lve is passed so evolve_fn can use encode/decode/init_population
        
        Returns:
            final population in original space (numpy array)
        """
        return evolve_fn(self.toolbox, self, pop_size, n_gen)
