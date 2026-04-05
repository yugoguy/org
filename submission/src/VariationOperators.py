from abc import ABC, abstractmethod
import numpy as np
import torch



class VariationOperator(ABC):
    """
    Abstract variation operator for evolutionary search.

    Each operator selects parents from the archive and produces
    n child candidates via its variation strategy.

    Args:
        greedy_mem: if True, select best fitness member in chosen bin.
                    if False (default), uniform random member.
    """

    def __init__(self, greedy_mem=False):
        self.name = "operator"
        self.greedy_mem = greedy_mem

    @abstractmethod
    def __call__(self, bm, n, latent_module=None):
        pass

    def _select_parent(self, bm):
        """Uniform bin, then uniform or greedy member. Returns dataset index."""
        bin_ids = list(bm.bins_idx.keys())
        bid = bin_ids[np.random.randint(len(bin_ids))]
        members = bm.bins_idx[bid]
        if self.greedy_mem:
            return min(members, key=lambda i: bm.fitnesses[i])
        return members[np.random.randint(len(members))]


class PSEMut(VariationOperator):
    """Gaussian mutation in parameter space."""

    def __init__(self, sigma=0.3, greedy_mem=False):
        super().__init__(greedy_mem=greedy_mem)
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


class PSELine(VariationOperator):
    """Line recombination: alpha * parent_a + (1-alpha) * parent_b."""

    def __init__(self, greedy_mem=False):
        super().__init__(greedy_mem=greedy_mem)
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


class LVEMut(VariationOperator):
    """Gaussian mutation in latent space: encode -> noise -> decode."""

    def __init__(self, sigma=0.1, greedy_mem=False):
        super().__init__(greedy_mem=greedy_mem)
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


class LVECross(VariationOperator):
    """Crossover in latent space: encode two parents, interpolate, decode."""

    def __init__(self, greedy_mem=False):
        super().__init__(greedy_mem=greedy_mem)
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
    """Sample z uniformly in [lo, hi] per dimension, then translate to x.
    Uses translate_fn if provided, otherwise latent_module.decode.

    Args:
        lo: lower bound per latent dimension
        hi: upper bound per latent dimension
        translate_fn: callable(z_tensor) -> x_tensor, or None
    """

    def __init__(self, lo=-2.0, hi=2.0, translate_fn=None):
        super().__init__()
        self.name = "std_support_lve"
        self.lo = lo
        self.hi = hi
        self.translate_fn = translate_fn

    def __call__(self, bm, n, latent_module=None):
        if latent_module is None or n == 0:
            return []
        device = next(latent_module.parameters()).device
        latent_dim = latent_module.latent_dim
        latent_module.eval()
        translate = self.translate_fn if self.translate_fn is not None else latent_module.decode
        with torch.no_grad():
            z = torch.rand(n, latent_dim, device=device) * (self.hi - self.lo) + self.lo
            decoded = translate(z)
        return [d.cpu().numpy() for d in decoded]
