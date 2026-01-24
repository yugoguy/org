#@title LVE

import numpy as np
import torch

class LVE:
    """
    Latent Variable Evolution main class.
    
    Args:
        data_generation: DataGeneration instance
        latent_module: LatentModule instance
        toolbox: DEAP toolbox for latent space GA
        device: 'cpu' or 'cuda'
    """
    
    def __init__(self, data_generation, latent_module, toolbox, device='cpu'):
        self.data_generation = data_generation
        self.latent_module = latent_module
        self.toolbox = toolbox
        self.device = device
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
    
    def evolve(self, pop_size, n_gen, evolve_fn):
        """
        Main LVE loop.
        
        Args:
            pop_size: population size
            n_gen: number of generations
            evolve_fn: function(toolbox, lve, pop_size, n_gen) -> final population
                       lve is passed so evolve_fn can use encode/decode
        
        Returns:
            final population in original space (numpy array)
        """
        return evolve_fn(self.toolbox, self, pop_size, n_gen)
