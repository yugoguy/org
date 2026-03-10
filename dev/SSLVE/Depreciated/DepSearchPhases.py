from abc import ABC, abstractmethod
import numpy as np
import torch
import bisect



class UniBinUniMemCMAMEimpPSE:
    """
    CMA-ME with Improvement Emitter in Parameter Space.

    Each sample() call runs n_emitters, each:
    1. Pick random occupied bin, random member as CMA-ES mean
    2. Run CMA-ES for n_generations with improvement-based selection
    3. Sample n_output fresh candidates from the final adapted distribution

    All emitters' outputs are stacked as the returned thetas.
    Fallback: init_fn (default: He-init) random samples when archive is empty.

    Args:
        agent_class: class with set_weights/act
        architecture: list of layer dims
        agent_kwargs: dict for agent constructor
        n_emitters: number of emitters per sample() call
        n_generations: CMA-ES generations per emitter
        sigma_init: initial CMA-ES step size
        lambda_: CMA-ES population size per generation (for internal selection)
        n_output: number of fresh samples per emitter to output
        n_init_samples: number of init samples when archive empty
        init_fn: callable() -> numpy array, custom init (default: He-init)
    """

    def __init__(self, agent_class, architecture, agent_kwargs=None,
                 n_emitters=5, n_generations=10, sigma_init=1.0,
                 lambda_=20, n_output=20, n_init_samples=200, separable=False,
                 init_fn=None):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}
        self.n_emitters = n_emitters
        self.n_generations = n_generations
        self.sigma_init = sigma_init
        self.lambda_ = lambda_
        self.mu = lambda_ // 2
        self.n_output = n_output
        self.n_init_samples = n_init_samples
        self.separable = separable
        self.init_fn = init_fn
        self.dim = self._weight_dim()

        self._init_cma_weights()

    def _init_cma_weights(self):
        """Precompute CMA-ES recombination weights and learning rates."""
        mu = self.mu
        weights = np.log(mu + 0.5) - np.log(np.arange(1, mu + 1))
        weights = weights / weights.sum()
        self.weights = weights
        self.mu_eff = 1.0 / np.sum(weights ** 2)

        n = self.dim
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

    def _init(self):
        if self.init_fn is not None:
            return self.init_fn()
        return self._he_init()

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
            list of n_output thetas sampled from the final adapted distribution.
        """
        bin_ids = list(bm.bins_idx.keys())
        bid = bin_ids[np.random.randint(len(bin_ids))]
        members = bm.bins_idx[bid]
        idx = members[np.random.randint(len(members))]
        mean = bm.dataset[idx].copy()

        sigma = self.sigma_init
        n = self.dim
        p_sigma = np.zeros(n)
        p_c = np.zeros(n)

        if self.separable:
            C_diag = np.ones(n)

            for gen in range(self.n_generations):
                D = np.sqrt(np.maximum(C_diag, 1e-20))

                candidates = []
                for _ in range(self.lambda_):
                    z = np.random.randn(n)
                    x = mean + sigma * D * z
                    candidates.append(x)

                eval_results = []
                for theta in candidates:
                    agent = self.make_agent(theta)
                    info = collector.collect(agent)
                    fitness = bm.fitness_fn(info)
                    descriptor = bm.behavior_descriptor.describe(info)
                    bin_id = bm.behavior_descriptor.discretize(descriptor)
                    score = self._score(bin_id, fitness, bm)
                    eval_results.append((score, np.random.random(), theta))

                eval_results.sort(key=lambda x: (-x[0], x[1]))
                selected_thetas = [s[2] for s in eval_results[:self.mu]]

                old_mean = mean.copy()
                mean = np.zeros(n)
                for i, theta in enumerate(selected_thetas):
                    mean += self.weights[i] * theta

                mean_diff = (mean - old_mean) / sigma
                inv_D = 1.0 / D
                p_sigma = ((1 - self.c_sigma) * p_sigma
                           + np.sqrt(self.c_sigma * (2 - self.c_sigma) * self.mu_eff)
                           * (inv_D * mean_diff))

                h_sigma = (np.linalg.norm(p_sigma)
                           / np.sqrt(1 - (1 - self.c_sigma) ** (2 * (gen + 1)))
                           < (1.4 + 2 / (n + 1)) * self.chi_n)

                p_c = ((1 - self.c_c) * p_c
                       + h_sigma * np.sqrt(self.c_c * (2 - self.c_c) * self.mu_eff)
                       * mean_diff)

                artmp = np.array([(theta - old_mean) / sigma for theta in selected_thetas])
                C_diag = ((1 - self.c_1 - self.c_mu_cov
                           + (1 - h_sigma) * self.c_1 * self.c_c * (2 - self.c_c)) * C_diag
                          + self.c_1 * p_c ** 2
                          + self.c_mu_cov * sum(self.weights[i] * artmp[i] ** 2
                                                for i in range(self.mu)))

                sigma *= np.exp((self.c_sigma / self.d_sigma)
                                * (np.linalg.norm(p_sigma) / self.chi_n - 1))

            D = np.sqrt(np.maximum(C_diag, 1e-20))
            output = []
            for _ in range(self.n_output):
                z = np.random.randn(n)
                x = mean + sigma * D * z
                output.append(x)

        else:
            C = np.eye(n)

            for gen in range(self.n_generations):
                eigvals, eigvecs = np.linalg.eigh(C)
                eigvals = np.maximum(eigvals, 1e-20)
                D = np.sqrt(eigvals)
                B = eigvecs

                candidates = []
                for _ in range(self.lambda_):
                    z = np.random.randn(n)
                    x = mean + sigma * (B @ (D * z))
                    candidates.append(x)

                eval_results = []
                for theta in candidates:
                    agent = self.make_agent(theta)
                    info = collector.collect(agent)
                    fitness = bm.fitness_fn(info)
                    descriptor = bm.behavior_descriptor.describe(info)
                    bin_id = bm.behavior_descriptor.discretize(descriptor)
                    score = self._score(bin_id, fitness, bm)
                    eval_results.append((score, np.random.random(), theta))

                eval_results.sort(key=lambda x: (-x[0], x[1]))
                selected_thetas = [s[2] for s in eval_results[:self.mu]]

                old_mean = mean.copy()
                mean = np.zeros(n)
                for i, theta in enumerate(selected_thetas):
                    mean += self.weights[i] * theta

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

                artmp = np.array([(theta - old_mean) / sigma for theta in selected_thetas])
                C = ((1 - self.c_1 - self.c_mu_cov
                      + (1 - h_sigma) * self.c_1 * self.c_c * (2 - self.c_c)) * C
                     + self.c_1 * np.outer(p_c, p_c)
                     + self.c_mu_cov * sum(self.weights[i] * np.outer(artmp[i], artmp[i])
                                           for i in range(self.mu)))

                sigma *= np.exp((self.c_sigma / self.d_sigma)
                                * (np.linalg.norm(p_sigma) / self.chi_n - 1))

            eigvals, eigvecs = np.linalg.eigh(C)
            eigvals = np.maximum(eigvals, 1e-20)
            D = np.sqrt(eigvals)
            B = eigvecs

            output = []
            for _ in range(self.n_output):
                z = np.random.randn(n)
                x = mean + sigma * (B @ (D * z))
                output.append(x)

        return output

    def sample(self, collector, behavior_matching, **kwargs):
        """
        Generate candidate thetas.

        Args:
            collector: Collector instance (needed for internal CMA-ES evaluation)
            behavior_matching: BehaviorMatching instance (read-only during emitting)

        Returns:
            list of numpy arrays (candidate thetas)
        """
        if len(behavior_matching.bins) == 0:
            return [self._init() for _ in range(self.n_init_samples)]

        all_thetas = []
        for _ in range(self.n_emitters):
            thetas = self._run_emitter(collector, behavior_matching)
            all_thetas.extend(thetas)

        return all_thetas



class UniBinUniMemCMAMEimpLVE:
    """
    CMA-ME with Improvement Emitter in Latent Space.

    Each sample() call runs n_emitters, each:
    1. Pick random occupied bin, random member theta
    2. Encode theta to latent mu as CMA-ES mean
    3. Run CMA-ES in latent space for n_generations with improvement-based selection
    4. Sample n_output fresh candidates from final adapted latent distribution, decode

    All emitters' decoded outputs are stacked as the returned thetas.
    Fallback: init_fn (default: He-init) random samples when no LM or archive empty.

    Args:
        agent_class: class with set_weights/act
        architecture: list of layer dims
        agent_kwargs: dict for agent constructor
        n_emitters: number of emitters per sample() call
        n_generations: CMA-ES generations per emitter
        sigma_init: initial CMA-ES step size in latent space
        lambda_: CMA-ES population size per generation
        n_output: number of fresh samples per emitter to output
        n_init_samples: number of init samples when archive/LM unavailable
        latent_dim: latent space dimensionality (must match latent_module)
        init_fn: callable() -> numpy array, custom init (default: He-init)
    """

    def __init__(self, agent_class, architecture, agent_kwargs=None,
                 n_emitters=5, n_generations=10, sigma_init=0.5,
                 lambda_=20, n_output=20, n_init_samples=200, latent_dim=128,
                 separable=False, init_fn=None):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}
        self.n_emitters = n_emitters
        self.n_generations = n_generations
        self.sigma_init = sigma_init
        self.lambda_ = lambda_
        self.mu = lambda_ // 2
        self.n_output = n_output
        self.n_init_samples = n_init_samples
        self.latent_dim = latent_dim
        self.separable = separable
        self.init_fn = init_fn

        self._init_cma_weights()

    def _init_cma_weights(self):
        """Precompute CMA-ES recombination weights and learning rates."""
        mu = self.mu
        weights = np.log(mu + 0.5) - np.log(np.arange(1, mu + 1))
        weights = weights / weights.sum()
        self.weights = weights
        self.mu_eff = 1.0 / np.sum(weights ** 2)

        n = self.latent_dim
        self.c_sigma = (self.mu_eff + 2) / (n + self.mu_eff + 5)
        self.d_sigma = 1 + 2 * max(0, np.sqrt((self.mu_eff - 1) / (n + 1)) - 1) + self.c_sigma
        self.c_c = (4 + self.mu_eff / n) / (n + 4 + 2 * self.mu_eff / n)
        self.c_1 = 2 / ((n + 1.3) ** 2 + self.mu_eff)
        self.c_mu_cov = min(1 - self.c_1,
                            2 * (self.mu_eff - 2 + 1 / self.mu_eff) / ((n + 2) ** 2 + self.mu_eff))
        self.chi_n = np.sqrt(n) * (1 - 1 / (4 * n) + 1 / (21 * n ** 2))

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

    def _decode(self, z_np, latent_module):
        """Decode latent vector to theta. No grad."""
        device = next(latent_module.parameters()).device
        with torch.no_grad():
            z = torch.tensor(z_np, dtype=torch.float32).unsqueeze(0).to(device)
            theta = latent_module.decode(z).squeeze(0).cpu().numpy()
        return theta

    def _decode_batch(self, z_batch_np, latent_module):
        """Decode batch of latent vectors to thetas. No grad."""
        device = next(latent_module.parameters()).device
        with torch.no_grad():
            z = torch.tensor(np.array(z_batch_np), dtype=torch.float32).to(device)
            thetas = latent_module.decode(z).cpu().numpy()
        return [thetas[i] for i in range(len(thetas))]

    def _encode(self, theta, latent_module):
        """Encode theta to latent mu. No grad."""
        device = next(latent_module.parameters()).device
        with torch.no_grad():
            x = torch.tensor(theta, dtype=torch.float32).unsqueeze(0).to(device)
            mu = latent_module.encode(x).squeeze(0).cpu().numpy()
        return mu

    def _run_emitter(self, collector, bm, latent_module):
        """
        Run one CMA-ES improvement emitter in latent space.

        Returns:
            list of n_output thetas decoded from the final adapted latent distribution.
        """
        bin_ids = list(bm.bins_idx.keys())
        bid = bin_ids[np.random.randint(len(bin_ids))]
        members = bm.bins_idx[bid]
        idx = members[np.random.randint(len(members))]
        theta_parent = bm.dataset[idx]

        mean = self._encode(theta_parent, latent_module)

        sigma = self.sigma_init
        n = self.latent_dim
        p_sigma = np.zeros(n)
        p_c = np.zeros(n)

        if self.separable:
            C_diag = np.ones(n)

            for gen in range(self.n_generations):
                D = np.sqrt(np.maximum(C_diag, 1e-20))

                z_candidates = []
                for _ in range(self.lambda_):
                    z_noise = np.random.randn(n)
                    z = mean + sigma * D * z_noise
                    z_candidates.append(z)

                thetas = self._decode_batch(z_candidates, latent_module)

                eval_results = []
                for i, theta in enumerate(thetas):
                    agent = self.make_agent(theta)
                    info = collector.collect(agent)
                    fitness = bm.fitness_fn(info)
                    descriptor = bm.behavior_descriptor.describe(info)
                    bin_id = bm.behavior_descriptor.discretize(descriptor)
                    score = self._score(bin_id, fitness, bm)
                    eval_results.append((score, np.random.random(), z_candidates[i]))

                eval_results.sort(key=lambda x: (-x[0], x[1]))
                selected_zs = [s[2] for s in eval_results[:self.mu]]

                old_mean = mean.copy()
                mean = np.zeros(n)
                for i, z in enumerate(selected_zs):
                    mean += self.weights[i] * z

                mean_diff = (mean - old_mean) / sigma
                inv_D = 1.0 / D
                p_sigma = ((1 - self.c_sigma) * p_sigma
                           + np.sqrt(self.c_sigma * (2 - self.c_sigma) * self.mu_eff)
                           * (inv_D * mean_diff))

                h_sigma = (np.linalg.norm(p_sigma)
                           / np.sqrt(1 - (1 - self.c_sigma) ** (2 * (gen + 1)))
                           < (1.4 + 2 / (n + 1)) * self.chi_n)

                p_c = ((1 - self.c_c) * p_c
                       + h_sigma * np.sqrt(self.c_c * (2 - self.c_c) * self.mu_eff)
                       * mean_diff)

                artmp = np.array([(z - old_mean) / sigma for z in selected_zs])
                C_diag = ((1 - self.c_1 - self.c_mu_cov
                           + (1 - h_sigma) * self.c_1 * self.c_c * (2 - self.c_c)) * C_diag
                          + self.c_1 * p_c ** 2
                          + self.c_mu_cov * sum(self.weights[i] * artmp[i] ** 2
                                                for i in range(self.mu)))

                sigma *= np.exp((self.c_sigma / self.d_sigma)
                                * (np.linalg.norm(p_sigma) / self.chi_n - 1))

            D = np.sqrt(np.maximum(C_diag, 1e-20))
            z_output = []
            for _ in range(self.n_output):
                z_noise = np.random.randn(n)
                z = mean + sigma * D * z_noise
                z_output.append(z)

        else:
            C = np.eye(n)

            for gen in range(self.n_generations):
                eigvals, eigvecs = np.linalg.eigh(C)
                eigvals = np.maximum(eigvals, 1e-20)
                D = np.sqrt(eigvals)
                B = eigvecs

                z_candidates = []
                for _ in range(self.lambda_):
                    z_noise = np.random.randn(n)
                    z = mean + sigma * (B @ (D * z_noise))
                    z_candidates.append(z)

                thetas = self._decode_batch(z_candidates, latent_module)

                eval_results = []
                for i, theta in enumerate(thetas):
                    agent = self.make_agent(theta)
                    info = collector.collect(agent)
                    fitness = bm.fitness_fn(info)
                    descriptor = bm.behavior_descriptor.describe(info)
                    bin_id = bm.behavior_descriptor.discretize(descriptor)
                    score = self._score(bin_id, fitness, bm)
                    eval_results.append((score, np.random.random(), z_candidates[i]))

                eval_results.sort(key=lambda x: (-x[0], x[1]))
                selected_zs = [s[2] for s in eval_results[:self.mu]]

                old_mean = mean.copy()
                mean = np.zeros(n)
                for i, z in enumerate(selected_zs):
                    mean += self.weights[i] * z

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

                artmp = np.array([(z - old_mean) / sigma for z in selected_zs])
                C = ((1 - self.c_1 - self.c_mu_cov
                      + (1 - h_sigma) * self.c_1 * self.c_c * (2 - self.c_c)) * C
                     + self.c_1 * np.outer(p_c, p_c)
                     + self.c_mu_cov * sum(self.weights[i] * np.outer(artmp[i], artmp[i])
                                           for i in range(self.mu)))

                sigma *= np.exp((self.c_sigma / self.d_sigma)
                                * (np.linalg.norm(p_sigma) / self.chi_n - 1))

            eigvals, eigvecs = np.linalg.eigh(C)
            eigvals = np.maximum(eigvals, 1e-20)
            D = np.sqrt(eigvals)
            B = eigvecs

            z_output = []
            for _ in range(self.n_output):
                z_noise = np.random.randn(n)
                z = mean + sigma * (B @ (D * z_noise))
                z_output.append(z)

        return self._decode_batch(z_output, latent_module)

    def sample(self, latent_module=None, collector=None, behavior_matching=None, **kwargs):
        """
        Generate candidate thetas.

        Args:
            latent_module: BetaVAE_SSLVE instance or None
            collector: Collector instance (needed for internal CMA-ES evaluation)
            behavior_matching: BehaviorMatching instance (read-only during emitting)

        Returns:
            list of numpy arrays (candidate thetas)
        """
        if latent_module is None or behavior_matching is None or len(behavior_matching.bins) == 0:
            return [self._init() for _ in range(self.n_init_samples)]

        latent_module.eval()
        all_thetas = []
        for _ in range(self.n_emitters):
            thetas = self._run_emitter(collector, behavior_matching, latent_module)
            all_thetas.extend(thetas)

        return all_thetas
