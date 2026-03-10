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
    def sample(self, **kwargs):
        """
        Generate candidate thetas.

        Returns:
            list of numpy arrays (candidate thetas)
        """
        pass


class UniBinUniMemLVE:
    """
    Search phase: uniform over bins, uniform within bin.
    Encodes selected theta via LM, mutates in latent space, decodes.
    Falls back to init_fn (default: He-init) random thetas if no LM available.

    Args:
        agent_class: class with architecture info
        architecture: list of layer dims
        agent_kwargs: dict for agent constructor (e.g. output_activation)
        mutation_sigma: noise std added in latent space
        n_samples: number of candidates to generate per call
        init_fn: callable() -> numpy array, custom init (default: He-init)
    """

    def __init__(self, agent_class, architecture, agent_kwargs=None,
                 mutation_sigma=0.1, n_samples=50, init_fn=None):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}
        self.mutation_sigma = mutation_sigma
        self.n_samples = n_samples
        self.init_fn = init_fn

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

    def _init(self):
        if self.init_fn is not None:
            return self.init_fn()
        return self._he_init()

    def _weight_dim(self):
        dim = 0
        for i in range(len(self.architecture) - 1):
            dim += self.architecture[i] * self.architecture[i + 1]
            dim += self.architecture[i + 1]
        return dim

    def sample(self, latent_module=None, behavior_matching=None, **kwargs):
        """
        Generate candidate thetas.

        If no LM: init_fn random thetas.
        If LM available: uniform sample from archive, encode (mu + sigma * noise),
        add mutation noise, decode.

        Args:
            latent_module: BetaVAE_SSLVE instance or None
            behavior_matching: MAPElitesBM instance or None

        Returns:
            list of numpy arrays (candidate thetas)
        """
        if latent_module is None or behavior_matching is None or len(behavior_matching.bins) == 0:
            return [self._init() for _ in range(self.n_samples)]

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
    Falls back to init_fn (default: He-init) random thetas if archive empty.

    Args:
        agent_class: class with architecture info
        architecture: list of layer dims
        agent_kwargs: dict for agent constructor
        mutation_sigma: noise std added in weight space
        n_samples: number of candidates per call
        init_fn: callable() -> numpy array, custom init (default: He-init)
    """

    def __init__(self, agent_class, architecture, agent_kwargs=None,
                 mutation_sigma=0.1, n_samples=50, init_fn=None):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}
        self.mutation_sigma = mutation_sigma
        self.n_samples = n_samples
        self.init_fn = init_fn

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

    def _init(self):
        if self.init_fn is not None:
            return self.init_fn()
        return self._he_init()

    def sample(self, behavior_matching=None, **kwargs):
        """
        Generate candidate thetas.

        If archive empty: init_fn random thetas.
        Otherwise: uniform bin -> uniform member -> Gaussian noise in weight space.

        Args:
            behavior_matching: MAPElitesBM instance or None

        Returns:
            list of numpy arrays
        """
        if behavior_matching is None or len(behavior_matching.bins) == 0:
            return [self._init() for _ in range(self.n_samples)]

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



class UniBinUniMemFixedMix:
    """
    Search phase: uniform over bins, uniform within bin.
    Three variation operators:
        - PSE mutation (parameter space Gaussian noise)
        - LVE mutation (latent space Gaussian noise)
        - LVE crossover (latent space random-alpha interpolation of two parents)
    Each can be turned off by setting its count to 0.

    Args:
        agent_class: class with architecture info
        architecture: list of layer dims
        agent_kwargs: dict for agent constructor
        mutation_sigma: noise std for both PSE and LVE mutation
        n_pse: number of PSE mutation samples
        n_lve_mutation: number of LVE mutation samples
        n_lve_crossover: number of LVE crossover samples
    """

    def __init__(self, agent_class, architecture, agent_kwargs=None,
                 mutation_sigma=0.3, n_pse=0, n_lve_mutation=0, n_lve_crossover=0,
                 init_fn=None):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}
        self.mutation_sigma = mutation_sigma
        self.n_pse = n_pse
        self.n_lve_mutation = n_lve_mutation
        self.n_lve_crossover = n_lve_crossover
        self.init_fn = init_fn

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

    def _init(self):
        if self.init_fn is not None:
            return self.init_fn()
        return self._he_init()

    def _select_parent(self, bm):
        """Uniform bin -> uniform member. Returns dataset index."""
        bin_ids = list(bm.bins_idx.keys())
        bid = bin_ids[np.random.randint(len(bin_ids))]
        members = bm.bins_idx[bid]
        return members[np.random.randint(len(members))]

    def sample(self, latent_module=None, behavior_matching=None, **kwargs):
        n_total = self.n_pse + self.n_lve_mutation + self.n_lve_crossover

        if latent_module is None or behavior_matching is None or len(behavior_matching.bins) == 0:
            return [self._init() for _ in range(max(n_total, 1))]

        bm = behavior_matching
        candidates = []

        # PSE mutation
        for _ in range(self.n_pse):
            idx = self._select_parent(bm)
            parent = bm.dataset[idx]
            child = parent + self.mutation_sigma * np.random.randn(len(parent))
            candidates.append(child)

        if self.n_lve_mutation == 0 and self.n_lve_crossover == 0:
            return candidates

        device = next(latent_module.parameters()).device
        latent_module.eval()

        # LVE mutation
        if self.n_lve_mutation > 0:
            indices = [self._select_parent(bm) for _ in range(self.n_lve_mutation)]
            thetas = np.array([bm.dataset[i] for i in indices])
            with torch.no_grad():
                x = torch.tensor(thetas, dtype=torch.float32).to(device)
                mu, logvar = latent_module.encode_dist(x)
                std = torch.exp(0.5 * logvar)
                z = mu + std * torch.randn_like(std)
                z = z + self.mutation_sigma * torch.randn_like(z)
                decoded = latent_module.decode(z)
            candidates.extend([d.cpu().numpy() for d in decoded])

        # LVE crossover
        if self.n_lve_crossover > 0:
            indices_a = [self._select_parent(bm) for _ in range(self.n_lve_crossover)]
            indices_b = [self._select_parent(bm) for _ in range(self.n_lve_crossover)]
            thetas_a = np.array([bm.dataset[i] for i in indices_a])
            thetas_b = np.array([bm.dataset[i] for i in indices_b])
            with torch.no_grad():
                xa = torch.tensor(thetas_a, dtype=torch.float32).to(device)
                xb = torch.tensor(thetas_b, dtype=torch.float32).to(device)
                mu_a = latent_module.encode(xa)
                mu_b = latent_module.encode(xb)
                alpha = torch.rand(self.n_lve_crossover, 1, device=device)
                z = alpha * mu_a + (1 - alpha) * mu_b
                decoded = latent_module.decode(z)
            candidates.extend([d.cpu().numpy() for d in decoded])

        return candidates



class UniBinUniMemBoltzmannMix:
    """
    Adaptive search phase using Boltzmann (softmax) exploration over three operators:
        - PSE mutation (parameter space Gaussian noise)
        - LVE mutation (latent space Gaussian noise)
        - LVE crossover (latent space interpolation)

    Warmup phase: pure PSE until archive reaches warmup_threshold.
    Adaptive phase: allocate n_total samples across operators using softmax
    over EMA of per-operator mean reward. Reward is read from bm.rewards,
    computed by BM when bm.compute_rewards is True.

    Args:
        agent_class: class with architecture info
        architecture: list of layer dims
        agent_kwargs: dict for agent constructor
        pse_sigma: noise std for PSE mutation in weight space
        lve_sigma: noise std for LVE mutation in latent space
        n_total: total samples per step
        warmup_threshold: archive size before LVE starts
        ema_alpha: EMA decay rate for reward tracking
        temperature: softmax temperature (higher = more uniform)
        min_proportion: minimum fraction of n_total per operator (prevents starvation)
        init_fn: callable() -> numpy array, custom init
    """

    def __init__(self, agent_class, architecture, agent_kwargs=None,
                 pse_sigma=0.2, lve_sigma=0.1, n_total=200, warmup_threshold=100,
                 ema_alpha=0.3, temperature=1.0, min_proportion=0.05,
                 init_fn=None):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}
        self.pse_sigma = pse_sigma
        self.lve_sigma = lve_sigma
        self.n_total = n_total
        self.warmup_threshold = warmup_threshold
        self.ema_alpha = ema_alpha
        self.temperature = temperature
        self.min_proportion = min_proportion
        self.init_fn = init_fn

        # EMA reward rates for [pse, lve_mutation, lve_crossover]
        self.ema_rates = np.array([1.0, 1.0, 1.0])
        self.prev_tags = None
        self.warmed_up = False

        # Allocation history for plotting
        self.allocation_history = []
        self.ema_history = []

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

    def _init(self):
        if self.init_fn is not None:
            return self.init_fn()
        return self._he_init()

    def _select_parent(self, bm):
        """Uniform bin -> uniform member. Returns dataset index."""
        bin_ids = list(bm.bins_idx.keys())
        bid = bin_ids[np.random.randint(len(bin_ids))]
        members = bm.bins_idx[bid]
        return members[np.random.randint(len(members))]

    def _softmax_allocation(self):
        """Compute per-operator sample counts via softmax over EMA rates."""
        logits = self.ema_rates / self.temperature
        logits = logits - np.max(logits)
        exp_logits = np.exp(logits)
        probs = exp_logits / np.sum(exp_logits)

        min_count = max(1, int(self.n_total * self.min_proportion))
        counts = np.maximum(np.round(probs * self.n_total).astype(int), min_count)

        diff = self.n_total - np.sum(counts)
        if diff > 0:
            best = np.argmax(probs)
            counts[best] += diff
        elif diff < 0:
            for _ in range(-diff):
                worst = np.argmax(counts - min_count)
                if counts[worst] > min_count:
                    counts[worst] -= 1

        return counts[0], counts[1], counts[2]

    def _update_ema(self, rewards, tags):
        """Update EMA rates from per-candidate rewards and operator tags."""
        operator_names = ['pse', 'lve_mut', 'lve_xo']
        for i, op in enumerate(operator_names):
            op_rewards = [r for r, t in zip(rewards, tags) if t == op]
            if len(op_rewards) > 0:
                mean_reward = np.mean(op_rewards)
                self.ema_rates[i] = (self.ema_alpha * mean_reward
                                     + (1 - self.ema_alpha) * self.ema_rates[i])

    def _record_allocation(self, n_pse, n_lve_mut, n_lve_xo):
        """Record allocation and EMA state, print current ratio."""
        self.allocation_history.append((n_pse, n_lve_mut, n_lve_xo))
        self.ema_history.append(tuple(self.ema_rates.copy()))
        total = n_pse + n_lve_mut + n_lve_xo
        print(f"  Operator ratio - PSE: {n_pse}/{total} ({n_pse/total:.0%}), "
              f"LVE_mut: {n_lve_mut}/{total} ({n_lve_mut/total:.0%}), "
              f"LVE_xo: {n_lve_xo}/{total} ({n_lve_xo/total:.0%})")

    def sample(self, latent_module=None, behavior_matching=None, **kwargs):
        bm = behavior_matching

        # Process rewards from previous step
        if self.prev_tags is not None and bm is not None and bm.rewards is not None:
            self._update_ema(bm.rewards, self.prev_tags)

        # No archive yet: random init
        if bm is None or len(bm.bins) == 0:
            self.prev_tags = None
            if bm is not None:
                bm.compute_rewards = False
            if latent_module is not None:
                latent_module.skip_training = True
            self._record_allocation(self.n_total, 0, 0)
            return [self._init() for _ in range(self.n_total)]

        # Check warmup
        if not self.warmed_up:
            if bm.archive_size() >= self.warmup_threshold:
                self.warmed_up = True
                bm.compute_rewards = True
                if latent_module is not None:
                    latent_module.skip_training = False
            else:
                bm.compute_rewards = False
                if latent_module is not None:
                    latent_module.skip_training = True
                candidates = []
                tags = []
                for _ in range(self.n_total):
                    idx = self._select_parent(bm)
                    parent = bm.dataset[idx]
                    child = parent + self.pse_sigma * np.random.randn(len(parent))
                    candidates.append(child)
                    tags.append('pse')
                self.prev_tags = tags
                self._record_allocation(self.n_total, 0, 0)
                return candidates

        # Adaptive phase: allocate via softmax
        n_pse, n_lve_mut, n_lve_xo = self._softmax_allocation()
        self._record_allocation(n_pse, n_lve_mut, n_lve_xo)

        candidates = []
        tags = []

        # PSE mutation
        for _ in range(n_pse):
            idx = self._select_parent(bm)
            parent = bm.dataset[idx]
            child = parent + self.pse_sigma * np.random.randn(len(parent))
            candidates.append(child)
            tags.append('pse')

        # LVE mutation and crossover need latent module
        if latent_module is not None and (n_lve_mut > 0 or n_lve_xo > 0):
            device = next(latent_module.parameters()).device
            latent_module.eval()

            if n_lve_mut > 0:
                indices = [self._select_parent(bm) for _ in range(n_lve_mut)]
                thetas = np.array([bm.dataset[i] for i in indices])
                with torch.no_grad():
                    x = torch.tensor(thetas, dtype=torch.float32).to(device)
                    mu, logvar = latent_module.encode_dist(x)
                    std = torch.exp(0.5 * logvar)
                    z = mu + std * torch.randn_like(std)
                    z = z + self.lve_sigma * torch.randn_like(z)
                    decoded = latent_module.decode(z)
                for d in decoded:
                    candidates.append(d.cpu().numpy())
                    tags.append('lve_mut')

            if n_lve_xo > 0:
                indices_a = [self._select_parent(bm) for _ in range(n_lve_xo)]
                indices_b = [self._select_parent(bm) for _ in range(n_lve_xo)]
                thetas_a = np.array([bm.dataset[i] for i in indices_a])
                thetas_b = np.array([bm.dataset[i] for i in indices_b])
                with torch.no_grad():
                    xa = torch.tensor(thetas_a, dtype=torch.float32).to(device)
                    xb = torch.tensor(thetas_b, dtype=torch.float32).to(device)
                    mu_a = latent_module.encode(xa)
                    mu_b = latent_module.encode(xb)
                    alpha = torch.rand(n_lve_xo, 1, device=device)
                    z = alpha * mu_a + (1 - alpha) * mu_b
                    decoded = latent_module.decode(z)
                for d in decoded:
                    candidates.append(d.cpu().numpy())
                    tags.append('lve_xo')
        else:
            # No latent module available, fill with PSE
            for _ in range(n_lve_mut + n_lve_xo):
                idx = self._select_parent(bm)
                parent = bm.dataset[idx]
                child = parent + self.pse_sigma * np.random.randn(len(parent))
                candidates.append(child)
                tags.append('pse')

        self.prev_tags = tags
        return candidates

    def plot_allocation(self, save_path=None):
        """Plot operator allocation ratio and EMA rates over steps."""
        import matplotlib.pyplot as plt

        if not self.allocation_history:
            print("No allocation history to plot.")
            return

        history = np.array(self.allocation_history, dtype=float)
        totals = history.sum(axis=1, keepdims=True)
        totals = np.maximum(totals, 1)
        ratios = history / totals

        steps = np.arange(len(ratios))

        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8), sharex=True)

        ax1.stackplot(steps, ratios[:, 0], ratios[:, 1], ratios[:, 2],
                       labels=['PSE', 'LVE mutation', 'LVE crossover'],
                       colors=['#2196F3', '#FF9800', '#4CAF50'], alpha=0.8)
        ax1.set_ylabel('Proportion')
        ax1.set_title('Operator Allocation Over Steps')
        ax1.legend(loc='upper right')
        ax1.set_ylim(0, 1)

        if self.ema_history:
            ema = np.array(self.ema_history)
            ax2.plot(steps, ema[:, 0], label='PSE', color='#2196F3')
            ax2.plot(steps, ema[:, 1], label='LVE mutation', color='#FF9800')
            ax2.plot(steps, ema[:, 2], label='LVE crossover', color='#4CAF50')
            ax2.set_ylabel('EMA Reward Rate')
            ax2.set_title('EMA Reward Rates Over Steps')
            ax2.legend(loc='upper right')

        ax2.set_xlabel('Step')
        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=150, bbox_inches='tight')
        plt.show()
