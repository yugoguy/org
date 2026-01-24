from abc import ABC, abstractmethod
import torch
import torch.nn as nn
import numpy as np

class LatentModule(ABC, nn.Module):
    """
    Abstract class for latent representation learning.
    
    Args:
        input_dim: dimension of original space
        latent_dim: dimension of latent space
    """
    
    def __init__(self, input_dim, latent_dim):
        super().__init__()
        self.input_dim = input_dim
        self.latent_dim = latent_dim
    
    @abstractmethod
    def encode(self, x):
        """Map from original space to latent space."""
        pass
    
    @abstractmethod
    def decode(self, z):
        """Map from latent space to original space."""
        pass
    
    @abstractmethod
    def loss(self, x, x_recon, **kwargs):
        """Compute loss for training."""
        pass
    
    def fit(self, dataset, epochs=100, batch_size=32, lr=1e-3, device='cpu', verbose=True):
        """
        Training loop.
        
        Args:
            dataset: list of numpy arrays
            epochs: number of training epochs
            batch_size: batch size
            lr: learning rate
            device: 'cpu' or 'cuda'
            verbose: print loss during training
        
        Returns:
            list of losses per epoch
        """
        self.to(device)
        self.train()
        
        # Convert dataset to tensor
        data = torch.tensor(np.array(dataset), dtype=torch.float32)
        data_loader = torch.utils.data.DataLoader(data, batch_size=batch_size, shuffle=True)
        
        optimizer = torch.optim.Adam(self.parameters(), lr=lr)
        losses = []
        
        for epoch in range(epochs):
            epoch_loss = 0.0
            for batch in data_loader:
                batch = batch.to(device)
                optimizer.zero_grad()
                
                z = self.encode(batch)
                x_recon = self.decode(z)
                loss = self.loss(batch, x_recon)
                
                loss.backward()
                optimizer.step()
                epoch_loss += loss.item()
            
            avg_loss = epoch_loss / len(data_loader)
            losses.append(avg_loss)
            
            if verbose and (epoch + 1) % 10 == 0:
                print(f"Epoch {epoch + 1}/{epochs}, Loss: {avg_loss:.6f}")
        
        self.eval()
        return losses
