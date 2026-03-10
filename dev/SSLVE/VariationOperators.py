from abc import ABC, abstractmethod
import numpy as np
import torch


class VariationOperator(ABC):
    """
    Abstract variation operator for evolutionary search.
    
    Each operator selects parents from the archive and produces
    n child candidates via its variation strategy.
    """

    def __init__(self):
        self.name = "operator"

    @abstractmethod
    def __call__(self, bm, n, latent_module=None):
        """
        Generate n candidates.

        Args:
            bm: BehaviorMatching instance (archive)
            n: number of candidates to produce
            latent_module: LatentModule instance or None

        Returns:
            list of numpy arrays (candidate thetas)
        """
        pass

    def _select_parent(self, bm):
        """Uniform bin -> uniform member. Returns dataset index."""
        bin_ids = list(bm.bins_idx.keys())
        bid = bin_ids[np.random.randint(len(bin_ids))]
        members = bm.bins_idx[bid]
        return members[np.random.randint(len(members))]


class UniBinUniMemPSEMut(VariationOperator):
    """Gaussian mutation in parameter space."""

    def __init__(self, sigma=0.3):
        super().__init__()
        self.name = "pse_mut"
        self.sigma = sigma

    def __call__(self, bm, n, latent_module=None):
        candidates = []
        for _ in range(n):
            idx = self._select_parent(bm)
            parent = bm.dataset[idx]
            child = parent + self.sigma * np.random.randn(len(parent))
            candidates.append(child)
        return candidates


class UniBinUniMemPSELine(VariationOperator):
    """Line recombination: alpha * parent_a + (1-alpha) * parent_b."""

    def __init__(self):
        super().__init__()
        self.name = "pse_line"

    def __call__(self, bm, n, latent_module=None):
        candidates = []
        for _ in range(n):
            idx_a = self._select_parent(bm)
            idx_b = self._select_parent(bm)
            a = bm.dataset[idx_a]
            b = bm.dataset[idx_b]
            alpha = np.random.rand()
            child = alpha * a + (1 - alpha) * b
            candidates.append(child)
        return candidates


class UniBinUniMemLVEMut(VariationOperator):
    """Gaussian mutation in latent space: encode -> noise -> decode."""

    def __init__(self, sigma=0.1):
        super().__init__()
        self.name = "lve_mut"
        self.sigma = sigma

    def __call__(self, bm, n, latent_module=None):
        if latent_module is None or n == 0:
            return []
        indices = [self._select_parent(bm) for _ in range(n)]
        thetas = np.array([bm.dataset[i] for i in indices])
        device = next(latent_module.parameters()).device
        latent_module.eval()
        with torch.no_grad():
            x = torch.tensor(thetas, dtype=torch.float32).to(device)
            mu, logvar = latent_module.encode_dist(x)
            std = torch.exp(0.5 * logvar)
            z = mu + std * torch.randn_like(std)
            z = z + self.sigma * torch.randn_like(z)
            decoded = latent_module.decode(z)
        return [d.cpu().numpy() for d in decoded]


class UniBinUniMemLVECross(VariationOperator):
    """Crossover in latent space: encode two parents, interpolate, decode."""

    def __init__(self):
        super().__init__()
        self.name = "lve_cross"

    def __call__(self, bm, n, latent_module=None):
        if latent_module is None or n == 0:
            return []
        indices_a = [self._select_parent(bm) for _ in range(n)]
        indices_b = [self._select_parent(bm) for _ in range(n)]
        thetas_a = np.array([bm.dataset[i] for i in indices_a])
        thetas_b = np.array([bm.dataset[i] for i in indices_b])
        device = next(latent_module.parameters()).device
        latent_module.eval()
        with torch.no_grad():
            xa = torch.tensor(thetas_a, dtype=torch.float32).to(device)
            xb = torch.tensor(thetas_b, dtype=torch.float32).to(device)
            mu_a = latent_module.encode(xa)
            mu_b = latent_module.encode(xb)
            alpha = torch.rand(n, 1, device=device)
            z = alpha * mu_a + (1 - alpha) * mu_b
            decoded = latent_module.decode(z)
        return [d.cpu().numpy() for d in decoded]


class StandardNormalSupportLVE(VariationOperator):
    """Sample z uniformly in [lo, hi] per dimension, then decode."""

    def __init__(self, lo=-2.0, hi=2.0):
        super().__init__()
        self.name = "std_support_lve"
        self.lo = lo
        self.hi = hi

    def __call__(self, bm, n, latent_module=None):
        if latent_module is None or n == 0:
            return []
        device = next(latent_module.parameters()).device
        latent_dim = latent_module.latent_dim
        latent_module.eval()
        with torch.no_grad():
            z = torch.rand(n, latent_dim, device=device) * (self.hi - self.lo) + self.lo
            decoded = latent_module.decode(z)
        return [d.cpu().numpy() for d in decoded]
