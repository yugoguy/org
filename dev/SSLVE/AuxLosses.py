from abc import ABC, abstractmethod
import torch
import torch.nn as nn
import numpy as np



class AuxLoss(ABC):
    """
    Abstract auxiliary loss for latent modules.
    Receives a context dict from the base model's fit loop.
    """

    def __init__(self):
        self.name = "aux"

    @abstractmethod
    def compute(self, **context):
        """
        Compute auxiliary loss.

        Args:
            **context: dict of available quantities from base model's training loop.

        Returns:
            scalar tensor
        """
        pass



class BinLatentKL(AuxLoss):
    """
    KL-based bin-mate alignment loss.
    KL(q(z|theta') || q(z|theta)) where theta' is stop-grad bin-mate.
    """

    def __init__(self):
        super().__init__()
        self.name = "bin_kl"

    def compute(self, **context):
        mu = context['mu']
        logvar = context['logvar']
        bin_ids_batch = context['bin_ids_batch']
        bins = context['bins']
        dataset = context['dataset']
        model = context['model']

        device = mu.device
        neighbor_list = []
        counts = []
        for bid, idx in zip(bin_ids_batch, context['batch_indices']):
            neighbor_indices = [j for j in bins[bid] if j != idx]
            counts.append(len(neighbor_indices))
            if neighbor_indices:
                neighbor_list.append(dataset[neighbor_indices])

        if not neighbor_list:
            return torch.tensor(0.0, device=device)

        stacked = torch.cat(neighbor_list, dim=0).to(device)
        with torch.no_grad():
            mu_n, logvar_n = model.encode_dist(stacked)

        total_neighbors = mu_n.size(0)
        if total_neighbors == 0:
            return torch.tensor(0.0, device=device)

        counts_tensor = torch.tensor(counts, dtype=torch.long, device=device)
        mu_b = torch.repeat_interleave(mu, counts_tensor, dim=0)
        logvar_b = torch.repeat_interleave(logvar, counts_tensor, dim=0)

        var_b = logvar_b.exp()
        var_n = logvar_n.exp()
        kl = 0.5 * (logvar_b - logvar_n + var_n / var_b
                     + (mu_n - mu_b).pow(2) / var_b - 1.0)
        kl_per_neighbor = kl.mean(dim=1)

        loss = torch.tensor(0.0, device=device)
        n_active = 0
        offset = 0
        for c in counts:
            if c > 0:
                loss = loss + kl_per_neighbor[offset:offset + c].mean()
                n_active += 1
            offset += c

        if n_active == 0:
            return torch.tensor(0.0, device=device)
        return loss / n_active



class BinPred(AuxLoss, nn.Module):
    """
    Auxiliary loss: predict bin center value from latent z via linear layer.
    Loss is MSE between predicted and target bin center, backpropagated through encoder.

    Args:
        behavior_descriptor: BD instance with bin_value(bin_id) method
        latent_dim: latent space dimension
        output_dim: dimension of bin center value (e.g. 2 for 2D end-effector)
    """

    def __init__(self, behavior_descriptor, latent_dim, output_dim):
        AuxLoss.__init__(self)
        nn.Module.__init__(self)
        self.name = "bin_pred"
        self.bd = behavior_descriptor
        self.predictor = nn.Linear(latent_dim, output_dim)

    def compute(self, **context):
        z = context['z']
        bin_ids_batch = context['bin_ids_batch']
        device = z.device

        targets = torch.tensor(
            np.array([self.bd.bin_value(bid) for bid in bin_ids_batch]),
            dtype=torch.float32,
            device=device,
        )

        pred = self.predictor(z)
        return nn.functional.mse_loss(pred, targets)
