from abc import ABC, abstractmethod
import numpy as np
import torch
import bisect

class SearchPhase(ABC):
    """
    Abstract class for search phase in SSLVE.
    Knows agent class and architecture. Generates candidate thetas
    and converts them to agents.

    Args:
        agent_class: class with set_weights(flat_weights) and act(obs)
        architecture: list of layer dims, e.g. [24, 64, 64, 4]
        agent_kwargs: dict passed to agent_class constructor (e.g. output_activation)
    """

    def __init__(self, agent_class, architecture, agent_kwargs=None):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}

    def make_agent(self, theta):
        """
        Convert flat weight vector to agent.

        Args:
            theta: numpy array of flat weights

        Returns:
            agent with weights set
        """
        agent = self.agent_class(self.architecture, **self.agent_kwargs)
        agent.set_weights(theta)
        return agent

    @abstractmethod
    def sample(self, latent_module=None, behavior_matching=None):
        """
        Generate candidate thetas.

        Args:
            latent_module: LatentModule instance (None if not yet trained)
            behavior_matching: BehaviorMatching instance (None at t=0)

        Returns:
            list of numpy arrays (candidate thetas)
        """
        pass


class UniBinUniMemLVE:
    """
    Search phase: uniform over bins, uniform within bin.
    Encodes selected theta via LM, mutates in latent space, decodes.
    Falls back to He-initialized random thetas if no LM available.

    Args:
        agent_class: class with architecture info
        architecture: list of layer dims
        agent_kwargs: dict for agent constructor (e.g. output_activation)
        mutation_sigma: noise std added in latent space
        n_samples: number of candidates to generate per call
    """

    def __init__(self, agent_class, architecture, agent_kwargs=None,
                 mutation_sigma=0.1, n_samples=50):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}
        self.mutation_sigma = mutation_sigma
        self.n_samples = n_samples

    def make_agent(self, theta):
        agent = self.agent_class(self.architecture, **self.agent_kwargs)
        agent.set_weights(theta)
        return agent

    def _he_init(self):
        """Generate a flat weight vector using He initialization."""
        parts = []
        for i in range(len(self.architecture) - 1):
            fan_in = self.architecture[i]
            fan_out = self.architecture[i + 1]
            std = np.sqrt(2.0 / fan_in)
            W = np.random.randn(fan_in * fan_out) * std
            b = np.zeros(fan_out)
            parts.append(W)
            parts.append(b)
        return np.concatenate(parts)

    def _weight_dim(self):
        dim = 0
        for i in range(len(self.architecture) - 1):
            dim += self.architecture[i] * self.architecture[i + 1]
            dim += self.architecture[i + 1]
        return dim

    def sample(self, latent_module=None, behavior_matching=None):
        """
        Generate candidate thetas.

        If no LM: He-initialized random thetas.
        If LM available: uniform sample from archive, encode (mu + sigma * noise),
        add mutation noise, decode.

        Args:
            latent_module: BetaVAE_SSLVE instance or None
            behavior_matching: MAPElitesBM instance or None

        Returns:
            list of numpy arrays (candidate thetas)
        """
        if latent_module is None or behavior_matching is None or len(behavior_matching.bins) == 0:
            return [self._he_init() for _ in range(self.n_samples)]

        # Uniform over bins, uniform within bin
        bin_ids = list(behavior_matching.bins_idx.keys())
        selected_indices = []
        for _ in range(self.n_samples):
            bid = bin_ids[np.random.randint(len(bin_ids))]
            members = behavior_matching.bins_idx[bid]
            idx = members[np.random.randint(len(members))]
            selected_indices.append(idx)

        # Encode selected thetas
        selected_thetas = np.array([behavior_matching.dataset[i] for i in selected_indices])
        device = next(latent_module.parameters()).device

        latent_module.eval()
        with torch.no_grad():
            x = torch.tensor(selected_thetas, dtype=torch.float32).to(device)
            mu, logvar = latent_module.encode_dist(x)
            std = torch.exp(0.5 * logvar)
            z = mu + std * torch.randn_like(std)
            # Add mutation noise
            z = z + self.mutation_sigma * torch.randn_like(z)
            decoded = latent_module.decode(z)

        return [d.cpu().numpy() for d in decoded]



class UniBinUniMemPSE:
    """
    Search phase: uniform over bins, uniform within bin.
    Mutates directly in parameter space (no latent module).
    Falls back to He-initialized random thetas if archive empty.

    Args:
        agent_class: class with architecture info
        architecture: list of layer dims
        agent_kwargs: dict for agent constructor
        mutation_sigma: noise std added in weight space
        n_samples: number of candidates per call
    """

    def __init__(self, agent_class, architecture, agent_kwargs=None,
                 mutation_sigma=0.1, n_samples=50):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}
        self.mutation_sigma = mutation_sigma
        self.n_samples = n_samples

    def make_agent(self, theta):
        agent = self.agent_class(self.architecture, **self.agent_kwargs)
        agent.set_weights(theta)
        return agent

    def _he_init(self):
        parts = []
        for i in range(len(self.architecture) - 1):
            fan_in = self.architecture[i]
            fan_out = self.architecture[i + 1]
            std = np.sqrt(2.0 / fan_in)
            W = np.random.randn(fan_in * fan_out) * std
            b = np.zeros(fan_out)
            parts.append(W)
            parts.append(b)
        return np.concatenate(parts)

    def sample(self, behavior_matching=None):
        """
        Generate candidate thetas.

        If archive empty: He-initialized random thetas.
        Otherwise: uniform bin -> uniform member -> Gaussian noise in weight space.

        Args:
            behavior_matching: MAPElitesBM instance or None

        Returns:
            list of numpy arrays
        """
        if behavior_matching is None or len(behavior_matching.bins) == 0:
            return [self._he_init() for _ in range(self.n_samples)]

        bin_ids = list(behavior_matching.bins_idx.keys())
        candidates = []
        for _ in range(self.n_samples):
            bid = bin_ids[np.random.randint(len(bin_ids))]
            members = behavior_matching.bins_idx[bid]
            idx = members[np.random.randint(len(members))]
            parent = behavior_matching.dataset[idx]
            child = parent + self.mutation_sigma * np.random.randn(len(parent))
            candidates.append(child)
        return candidates



class UniBinUniMemCMAMEimpPSE:
    """
    CMA-ME with Improvement Emitter in Parameter Space.

    Each sample() call runs n_emitters, each:
    1. Pick random occupied bin, random member as CMA-ES mean
    2. Run CMA-ES for n_generations with improvement-based selection
    3. Output top mu (=lambda_//2) from final generation

    All emitters' outputs are stacked as the candidates.
    Returns (thetas, infos) since collection happens inside emitters.

    Fallback: He-init random samples when archive is empty.

    Args:
        agent_class: class with set_weights/act
        architecture: list of layer dims
        agent_kwargs: dict for agent constructor
        n_emitters: number of emitters per sample() call
        n_generations: CMA-ES generations per emitter
        sigma_init: initial CMA-ES step size
        lambda_: CMA-ES population size per emitter
        n_init_samples: number of He-init samples when archive empty
    """

    def __init__(self, agent_class, architecture, agent_kwargs=None,
                 n_emitters=5, n_generations=10, sigma_init=1.0,
                 lambda_=20, n_init_samples=200):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}
        self.n_emitters = n_emitters
        self.n_generations = n_generations
        self.sigma_init = sigma_init
        self.lambda_ = lambda_
        self.mu = lambda_ // 2
        self.n_init_samples = n_init_samples
        self.dim = self._weight_dim()

        # CMA-ES weights for weighted recombination
        self._init_cma_weights()

    def _init_cma_weights(self):
        """Precompute CMA-ES recombination weights and learning rates."""
        mu = self.mu
        weights = np.log(mu + 0.5) - np.log(np.arange(1, mu + 1))
        weights = weights / weights.sum()
        self.weights = weights
        self.mu_eff = 1.0 / np.sum(weights ** 2)

        n = self.dim
        # Learning rates
        self.c_sigma = (self.mu_eff + 2) / (n + self.mu_eff + 5)
        self.d_sigma = 1 + 2 * max(0, np.sqrt((self.mu_eff - 1) / (n + 1)) - 1) + self.c_sigma
        self.c_c = (4 + self.mu_eff / n) / (n + 4 + 2 * self.mu_eff / n)
        self.c_1 = 2 / ((n + 1.3) ** 2 + self.mu_eff)
        self.c_mu_cov = min(1 - self.c_1,
                            2 * (self.mu_eff - 2 + 1 / self.mu_eff) / ((n + 2) ** 2 + self.mu_eff))
        self.chi_n = np.sqrt(n) * (1 - 1 / (4 * n) + 1 / (21 * n ** 2))

    def _weight_dim(self):
        dim = 0
        for i in range(len(self.architecture) - 1):
            dim += self.architecture[i] * self.architecture[i + 1]
            dim += self.architecture[i + 1]
        return dim

    def make_agent(self, theta):
        agent = self.agent_class(self.architecture, **self.agent_kwargs)
        agent.set_weights(theta)
        return agent

    def _he_init(self):
        parts = []
        for i in range(len(self.architecture) - 1):
            fan_in = self.architecture[i]
            fan_out = self.architecture[i + 1]
            std = np.sqrt(2.0 / fan_in)
            W = np.random.randn(fan_in * fan_out) * std
            b = np.zeros(fan_out)
            parts.append(W)
            parts.append(b)
        return np.concatenate(parts)

    def _score(self, bin_id, fitness, bm):
        """
        Improvement score: read-only check against bm archive.

        Returns:
            1.0 if empty bin,
            1/(rank+1) if would enter top_k (rank 0 = best),
            0.0 otherwise.
        """
        if bin_id not in bm.bins:
            return 1.0
        existing = sorted([f for _, f in bm.bins[bin_id]])
        rank = bisect.bisect_left(existing, fitness)
        if rank < bm.top_k:
            return 1.0 / (rank + 1)
        return 0.0

    def _run_emitter(self, collector, bm):
        """
        Run one CMA-ES improvement emitter.

        Returns:
            (thetas, infos): lists of length mu, the selected individuals
                from the final generation.
        """
        # Pick random bin, random member
        bin_ids = list(bm.bins_idx.keys())
        bid = bin_ids[np.random.randint(len(bin_ids))]
        members = bm.bins_idx[bid]
        idx = members[np.random.randint(len(members))]
        mean = bm.dataset[idx].copy()

        # CMA-ES state
        sigma = self.sigma_init
        n = self.dim
        C = np.eye(n)
        p_sigma = np.zeros(n)
        p_c = np.zeros(n)

        last_selected_thetas = None
        last_selected_infos = None

        for gen in range(self.n_generations):
            # Sample lambda candidates
            # Eigendecomposition for sampling
            eigvals, eigvecs = np.linalg.eigh(C)
            eigvals = np.maximum(eigvals, 1e-20)
            D = np.sqrt(eigvals)
            B = eigvecs

            candidates = []
            z_vectors = []
            for _ in range(self.lambda_):
                z = np.random.randn(n)
                x = mean + sigma * (B @ (D * z))
                candidates.append(x)
                z_vectors.append(z)

            # Evaluate each candidate
            eval_results = []  # (score, theta, info, random_tiebreak)
            for theta in candidates:
                agent = self.make_agent(theta)
                info = collector.collect(agent)
                fitness = bm.fitness_fn(info)
                descriptor = bm.behavior_descriptor.describe(info)
                bin_id = bm.behavior_descriptor.discretize(descriptor)
                score = self._score(bin_id, fitness, bm)
                eval_results.append((score, np.random.random(), theta, info))

            # Rank: higher score better, ties broken randomly
            eval_results.sort(key=lambda x: (-x[0], x[1]))

            # Select top mu
            selected = eval_results[:self.mu]
            selected_thetas = [s[2] for s in selected]
            selected_infos = [s[3] for s in selected]

            last_selected_thetas = selected_thetas
            last_selected_infos = selected_infos

            # CMA-ES update
            old_mean = mean.copy()
            # Weighted mean
            mean = np.zeros(n)
            for i, theta in enumerate(selected_thetas):
                mean += self.weights[i] * theta

            # Evolution paths
            invsqrtC = B @ np.diag(1.0 / D) @ B.T
            mean_diff = (mean - old_mean) / sigma
            p_sigma = ((1 - self.c_sigma) * p_sigma
                       + np.sqrt(self.c_sigma * (2 - self.c_sigma) * self.mu_eff)
                       * (invsqrtC @ mean_diff))

            h_sigma = (np.linalg.norm(p_sigma)
                       / np.sqrt(1 - (1 - self.c_sigma) ** (2 * (gen + 1)))
                       < (1.4 + 2 / (n + 1)) * self.chi_n)

            p_c = ((1 - self.c_c) * p_c
                   + h_sigma * np.sqrt(self.c_c * (2 - self.c_c) * self.mu_eff)
                   * mean_diff)

            # Covariance update
            artmp = np.array([(theta - old_mean) / sigma for theta in selected_thetas])
            C = ((1 - self.c_1 - self.c_mu_cov + (1 - h_sigma) * self.c_1 * self.c_c * (2 - self.c_c)) * C
                 + self.c_1 * np.outer(p_c, p_c)
                 + self.c_mu_cov * sum(self.weights[i] * np.outer(artmp[i], artmp[i])
                                       for i in range(self.mu)))

            # Sigma update
            sigma *= np.exp((self.c_sigma / self.d_sigma)
                            * (np.linalg.norm(p_sigma) / self.chi_n - 1))

        return last_selected_thetas, last_selected_infos

    def sample(self, collector, behavior_matching):
        """
        Generate candidate thetas with cached infos.

        Args:
            collector: Collector instance
            behavior_matching: BehaviorMatching instance (read-only during emitting)

        Returns:
            (thetas, infos): lists of numpy arrays and info dicts
        """
        if len(behavior_matching.bins) == 0:
            # Fallback: He-init, collect info for each
            thetas = [self._he_init() for _ in range(self.n_init_samples)]
            infos = []
            for theta in thetas:
                agent = self.make_agent(theta)
                info = collector.collect(agent)
                infos.append(info)
            return thetas, infos

        all_thetas = []
        all_infos = []
        for _ in range(self.n_emitters):
            thetas, infos = self._run_emitter(collector, behavior_matching)
            all_thetas.extend(thetas)
            all_infos.extend(infos)

        return all_thetas, all_infos
