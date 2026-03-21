import os
import numpy as np
import cma
import bisect
import time
import matplotlib.pyplot as plt


class CMAMEEmitter:
    """
    Single persistent CMA-ME improvement emitter.
    Uses pycma for CMA-ES distribution management.
    Two-stage improvement ranking per Fontaine et al. 2020.

    Args:
        dim: solution dimensionality
        sigma_init: initial CMA-ES step size
        popsize: CMA-ES population size (lambda)
        greedy_mem: if True, pick best fitness member in bin for restart
    """

    def __init__(self, dim, sigma_init=1.0, popsize=20, greedy_mem=False):
        self.dim = dim
        self.sigma_init = sigma_init
        self.popsize = popsize
        self.greedy_mem = greedy_mem
        self.es = None

    def _select_parent(self, bm):
        """Random bin, greedy or random member. Returns theta."""
        bin_ids = list(bm.bins_idx.keys())
        bid = bin_ids[np.random.randint(len(bin_ids))]
        members = bm.bins_idx[bid]
        if self.greedy_mem:
            idx = min(members, key=lambda i: bm.fitnesses[i])
        else:
            idx = members[np.random.randint(len(members))]
        return bm.dataset[idx].copy()

    def _init_cma(self, mean):
        """Initialize pycma instance from mean."""
        self.es = cma.CMAEvolutionStrategy(
            mean, self.sigma_init,
            {'popsize': self.popsize, 'verb_disp': 0, 'verbose': -9,
             'CMA_active': True}
        )

    def _improvement_score(self, theta, info, bm):
        """
        Compute improvement status and delta for a candidate.

        Returns:
            status: 2 = new cell, 1 = improved existing, 0 = no improvement
            delta: fitness improvement value
            bin_id: discretized bin
            fitness: candidate fitness
        """
        descriptor = bm.behavior_descriptor.describe(info)
        bin_id = bm.behavior_descriptor.discretize(descriptor)
        fitness = bm.fitness_fn(info)

        if bin_id not in bm.bins:
            return 2, fitness, bin_id, fitness

        existing = sorted([f for _, f in bm.bins[bin_id]])
        if len(existing) < bm.top_k:
            return 2, fitness, bin_id, fitness
        if fitness < existing[-1]:
            return 1, existing[-1] - fitness, bin_id, fitness

        return 0, 0.0, bin_id, fitness

    def step(self, collector, bm, make_agent):
        """
        One generation: ask, evaluate, update archive, rank, tell.
        Restart if zero improvements.

        Args:
            collector: Collector instance
            bm: MAPElitesBM instance (updated in place)
            make_agent: callable(theta) -> agent

        Returns:
            n_evals: number of evaluations performed
        """
        if self.es is None or self.es.stop():
            mean = self._select_parent(bm)
            self._init_cma(mean)

        candidates = self.es.ask()
        n_evals = len(candidates)

        # Evaluate and score
        results = []
        for x in candidates:
            theta = np.array(x)
            agent = make_agent(theta)
            info = collector.collect(agent)
            status, delta, bin_id, fitness = self._improvement_score(theta, info, bm)
            results.append((theta, info, status, delta, bin_id, fitness))

        # Update archive for improving candidates
        improving = [(theta, info, status, delta, bin_id, fitness)
                     for theta, info, status, delta, bin_id, fitness in results
                     if status > 0]

        for theta, info, status, delta, bin_id, fitness in improving:
            if bin_id not in bm.bins:
                bm.bins[bin_id] = []
            bm.bins[bin_id].append((theta, fitness))
            if len(bm.bins[bin_id]) > bm.top_k:
                bm.bins[bin_id].sort(key=lambda x: x[1])
                bm.bins[bin_id] = bm.bins[bin_id][:bm.top_k]

        if improving:
            bm._rebuild()

        # Two-stage improvement ranking for CMA-ES tell
        if len(improving) > 0:
            # Build rank index for each candidate (lower = better for pycma)
            # Stage 1: new cells sorted by fitness (ascending)
            new_cells = [(i, fitness) for i, (theta, info, status, delta, bin_id, fitness)
                         in enumerate(results) if status == 2]
            new_cells.sort(key=lambda x: x[1])

            # Stage 2: improved existing sorted by delta (descending)
            improved_existing = [(i, delta) for i, (theta, info, status, delta, bin_id, fitness)
                                 in enumerate(results) if status == 1]
            improved_existing.sort(key=lambda x: -x[1])

            # Stage 3: non-improving (order doesn't matter)
            not_improving = [i for i, (theta, info, status, delta, bin_id, fitness)
                            in enumerate(results) if status == 0]

            # Assign synthetic fitness: ranked order -> 0, 1, 2, ...
            rank_order = ([idx for idx, _ in new_cells]
                          + [idx for idx, _ in improved_existing]
                          + not_improving)

            fitnesses_for_tell = [0.0] * len(candidates)
            for rank, idx in enumerate(rank_order):
                fitnesses_for_tell[idx] = float(rank)

            self.es.tell(candidates, fitnesses_for_tell)
        else:
            # No improvements: restart
            mean = self._select_parent(bm)
            self._init_cma(mean)

        return n_evals


class CMAME:
    """
    CMA-ME with Improvement Emitters (Fontaine et al. 2020).

    Multiple persistent emitters share one archive. Each step,
    each emitter runs one generation. Archive updated immediately.

    Args:
        agent_class: class with set_weights/act
        architecture: architecture spec
        agent_kwargs: dict for agent constructor
        collector: Collector instance
        behavior_matching: MAPElitesBM instance
        n_emitters: number of emitters
        sigma_init: initial CMA-ES step size
        popsize: CMA-ES population size per emitter
        greedy_mem: greedy member selection for restarts
        n_init_samples: random samples when archive empty
        init_fn: custom init function or None (default: He-init)
    """

    def __init__(self, agent_class, architecture, agent_kwargs=None,
                 collector=None, behavior_matching=None,
                 n_emitters=5, sigma_init=1.0, popsize=20,
                 greedy_mem=False, n_init_samples=200, init_fn=None):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}
        self.collector = collector
        self.bm = behavior_matching
        self.n_emitters = n_emitters
        self.n_init_samples = n_init_samples
        self.init_fn = init_fn

        self.dim = self._weight_dim()
        self.emitters = [
            CMAMEEmitter(self.dim, sigma_init, popsize, greedy_mem)
            for _ in range(n_emitters)
        ]

        self.history = {
            'fitness_min': [],
            'fitness_mean': [],
            'fitness_max': [],
            'coverage': [],
            'archive_size': [],
            'qd_score': [],
            'cumulative_evals': [],
        }
        self.total_evals = 0

    def _weight_dim(self):
        if isinstance(self.architecture, int):
            return self.architecture
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
        arch = self.architecture if isinstance(self.architecture, list) else [self.architecture]
        for i in range(len(arch) - 1):
            fan_in = arch[i]
            fan_out = arch[i + 1]
            std = np.sqrt(2.0 / fan_in)
            W = np.random.randn(fan_in * fan_out) * std
            b = np.zeros(fan_out)
            parts.append(W)
            parts.append(b)
        if len(arch) == 1:
            return np.random.randn(arch[0]) * 0.1
        return np.concatenate(parts)

    def _init(self):
        if self.init_fn is not None:
            return self.init_fn()
        return self._he_init()

    def _init_step(self):
        """Random init when archive is empty."""
        n_evals = 0
        thetas = [self._init() for _ in range(self.n_init_samples)]
        infos = []
        t_start = time.time()
        for i, theta in enumerate(thetas):
            agent = self.make_agent(theta)
            info = self.collector.collect(agent)
            infos.append(info)
            n_evals += 1
            elapsed = time.time() - t_start
            rate = (i + 1) / elapsed if elapsed > 0 else 0
            eta = (self.n_init_samples - i - 1) / rate if rate > 0 else 0
            print(f"\rInit: {i+1}/{self.n_init_samples} [{elapsed:.0f}s elapsed, {eta:.0f}s remaining]", end="", flush=True)
        print()
        self.bm.update(thetas, infos)
        return n_evals

    def step(self):
        """
        One CMAME step. If archive empty, random init.
        Otherwise each emitter runs one generation.

        Returns:
            n_evals: evaluations this step
        """
        if len(self.bm.bins) == 0:
            n_evals = self._init_step()
        else:
            n_evals = 0
            for i, emitter in enumerate(self.emitters):
                evals = emitter.step(self.collector, self.bm, self.make_agent)
                n_evals += evals

        self.total_evals += n_evals

        f_min, f_mean, f_max = self.bm.fitness_stats()
        self.history['fitness_min'].append(f_min)
        self.history['fitness_mean'].append(f_mean)
        self.history['fitness_max'].append(f_max)
        self.history['coverage'].append(self.bm.coverage())
        self.history['archive_size'].append(self.bm.archive_size())
        self.history['qd_score'].append(self.bm.qd_score())
        self.history['cumulative_evals'].append(self.total_evals)

        print(f"Archive: {self.bm.archive_size()}, "
              f"Bins: {len(self.bm.bins)}, "
              f"Coverage: {self.bm.coverage():.4f}, "
              f"Fitness min/mean/max: {f_min:.2f}/{f_mean:.2f}/{f_max:.2f}, "
              f"QD-score: {self.bm.qd_score():.4f}, "
              f"Evals: {self.total_evals}")

        return n_evals

    def run(self, n_steps):
        """
        Run CMAME for n_steps.

        Args:
            n_steps: number of steps
        """
        for t in range(n_steps):
            print(f"\n--- CMAME Step {t+1}/{n_steps} ---")
            self.step()

    def plot_history(self, save_path=None):
        if not self.history['fitness_min']:
            print("No history available.")
            return

        evals = self.history['cumulative_evals']
        fig, axes = plt.subplots(2, 2, figsize=(14, 10))

        ax = axes[0, 0]
        ax.plot(evals, self.history['fitness_min'], label='Min')
        ax.plot(evals, self.history['fitness_mean'], label='Mean')
        ax.plot(evals, self.history['fitness_max'], label='Max')
        ax.set_xlabel('Evaluations')
        ax.set_ylabel('Fitness')
        ax.set_title('Fitness over Evaluations')
        ax.legend()

        ax = axes[0, 1]
        ax.plot(evals, self.history['coverage'])
        ax.set_xlabel('Evaluations')
        ax.set_ylabel('Coverage')
        ax.set_title('Behavior Coverage')

        ax = axes[1, 0]
        ax.plot(evals, self.history['archive_size'])
        ax.set_xlabel('Evaluations')
        ax.set_ylabel('Size')
        ax.set_title('Archive Size')

        ax = axes[1, 1]
        ax.plot(evals, self.history['qd_score'])
        ax.set_xlabel('Evaluations')
        ax.set_ylabel('QD-score')
        ax.set_title('QD-score')

        plt.tight_layout()
        if save_path:
            plt.savefig(save_path, bbox_inches='tight')
        else:
            plt.show()
        plt.close()


def save_cmame_checkpoint(path, cmame):
    """
    Save CMAME checkpoint.

    Args:
        path: directory path (created if not exists)
        cmame: CMAME instance
    """
    os.makedirs(path, exist_ok=True)
    bm = cmame.bm
    np.savez(path + 'bm_data.npz',
             dataset=np.array(bm.dataset),
             fitnesses=np.array(bm.fitnesses),
             bin_ids=np.array(bm.bin_ids, dtype=object))
    np.save(path + 'history.npy', cmame.history)
    np.save(path + 'total_evals.npy', cmame.total_evals)


def load_cmame_checkpoint(path, cmame):
    """
    Load CMAME checkpoint. Emitters are reset (new CMA-ES instances
    will be initialized from archive on next step).

    Args:
        path: directory path
        cmame: CMAME instance (modified in place)
    """
    bm = cmame.bm
    data = np.load(path + 'bm_data.npz', allow_pickle=True)
    dataset = list(data['dataset'])
    fitnesses = list(data['fitnesses'])
    bin_ids = list(data['bin_ids'])

    bm.bins = {}
    for theta, fitness, bid in zip(dataset, fitnesses, bin_ids):
        bid = tuple(bid) if isinstance(bid, np.ndarray) else bid
        if bid not in bm.bins:
            bm.bins[bid] = []
        bm.bins[bid].append((theta, float(fitness)))
    bm._rebuild()

    loaded_history = np.load(path + 'history.npy', allow_pickle=True).item()
    for k in cmame.history:
        if k in loaded_history:
            cmame.history[k] = loaded_history[k]

    cmame.total_evals = int(np.load(path + 'total_evals.npy'))

    # Reset emitters — they will reinitialize from archive
    for emitter in cmame.emitters:
        emitter.es = None

    def make_agent(self, theta):
        agent = self.agent_class(self.architecture, **self.agent_kwargs)
        agent.set_weights(theta)
        return agent

    def _he_init(self):
        parts = []
        arch = self.architecture if isinstance(self.architecture, list) else [self.architecture]
        for i in range(len(arch) - 1):
            fan_in = arch[i]
            fan_out = arch[i + 1]
            std = np.sqrt(2.0 / fan_in)
            W = np.random.randn(fan_in * fan_out) * std
            b = np.zeros(fan_out)
            parts.append(W)
            parts.append(b)
        if len(arch) == 1:
            return np.random.randn(arch[0]) * 0.1
        return np.concatenate(parts)

    def _init(self):
        if self.init_fn is not None:
            return self.init_fn()
        return self._he_init()

    def _init_step(self):
        """Random init when archive is empty."""
        n_evals = 0
        thetas = [self._init() for _ in range(self.n_init_samples)]
        infos = []
        t_start = time.time()
        for i, theta in enumerate(thetas):
            agent = self.make_agent(theta)
            info = self.collector.collect(agent)
            infos.append(info)
            n_evals += 1
            elapsed = time.time() - t_start
            rate = (i + 1) / elapsed if elapsed > 0 else 0
            eta = (self.n_init_samples - i - 1) / rate if rate > 0 else 0
            print(f"\rInit: {i+1}/{self.n_init_samples} [{elapsed:.0f}s elapsed, {eta:.0f}s remaining]", end="", flush=True)
        print()
        self.bm.update(thetas, infos)
        return n_evals

    def step(self):
        """
        One CMAME step. If archive empty, random init.
        Otherwise each emitter runs one generation.

        Returns:
            n_evals: evaluations this step
        """
        if len(self.bm.bins) == 0:
            n_evals = self._init_step()
        else:
            n_evals = 0
            for i, emitter in enumerate(self.emitters):
                evals = emitter.step(self.collector, self.bm, self.make_agent)
                n_evals += evals

        self.total_evals += n_evals

        f_min, f_mean, f_max = self.bm.fitness_stats()
        self.history['fitness_min'].append(f_min)
        self.history['fitness_mean'].append(f_mean)
        self.history['fitness_max'].append(f_max)
        self.history['coverage'].append(self.bm.coverage())
        self.history['archive_size'].append(self.bm.archive_size())
        self.history['qd_score'].append(self.bm.qd_score())
        self.history['cumulative_evals'].append(self.total_evals)

        print(f"Archive: {self.bm.archive_size()}, "
              f"Bins: {len(self.bm.bins)}, "
              f"Coverage: {self.bm.coverage():.4f}, "
              f"Fitness min/mean/max: {f_min:.2f}/{f_mean:.2f}/{f_max:.2f}, "
              f"QD-score: {self.bm.qd_score():.4f}, "
              f"Evals: {self.total_evals}")

        return n_evals

    def run(self, n_steps):
        """
        Run CMAME for n_steps.

        Args:
            n_steps: number of steps
        """
        for t in range(n_steps):
            print(f"\n--- CMAME Step {t+1}/{n_steps} ---")
            self.step()

    def plot_history(self, save_path=None):
        if not self.history['fitness_min']:
            print("No history available.")
            return

        evals = self.history['cumulative_evals']
        fig, axes = plt.subplots(2, 2, figsize=(14, 10))

        ax = axes[0, 0]
        ax.plot(evals, self.history['fitness_min'], label='Min')
        ax.plot(evals, self.history['fitness_mean'], label='Mean')
        ax.plot(evals, self.history['fitness_max'], label='Max')
        ax.set_xlabel('Evaluations')
        ax.set_ylabel('Fitness')
        ax.set_title('Fitness over Evaluations')
        ax.legend()

        ax = axes[0, 1]
        ax.plot(evals, self.history['coverage'])
        ax.set_xlabel('Evaluations')
        ax.set_ylabel('Coverage')
        ax.set_title('Behavior Coverage')

        ax = axes[1, 0]
        ax.plot(evals, self.history['archive_size'])
        ax.set_xlabel('Evaluations')
        ax.set_ylabel('Size')
        ax.set_title('Archive Size')

        ax = axes[1, 1]
        ax.plot(evals, self.history['qd_score'])
        ax.set_xlabel('Evaluations')
        ax.set_ylabel('QD-score')
        ax.set_title('QD-score')

        plt.tight_layout()
        if save_path:
            plt.savefig(save_path, bbox_inches='tight')
        else:
            plt.show()
        plt.close()


def save_cmame_checkpoint(path, cmame):
    """
    Save CMAME checkpoint.

    Args:
        path: directory path (created if not exists)
        cmame: CMAME instance
    """
    os.makedirs(path, exist_ok=True)
    bm = cmame.bm
    np.savez(path + 'bm_data.npz',
             dataset=np.array(bm.dataset),
             fitnesses=np.array(bm.fitnesses),
             bin_ids=np.array(bm.bin_ids, dtype=object))
    np.save(path + 'history.npy', cmame.history)
    np.save(path + 'total_evals.npy', cmame.total_evals)


def load_cmame_checkpoint(path, cmame):
    """
    Load CMAME checkpoint. Emitters are reset (new CMA-ES instances
    will be initialized from archive on next step).

    Args:
        path: directory path
        cmame: CMAME instance (modified in place)
    """
    bm = cmame.bm
    data = np.load(path + 'bm_data.npz', allow_pickle=True)
    dataset = list(data['dataset'])
    fitnesses = list(data['fitnesses'])
    bin_ids = list(data['bin_ids'])

    bm.bins = {}
    for theta, fitness, bid in zip(dataset, fitnesses, bin_ids):
        bid = tuple(bid) if isinstance(bid, np.ndarray) else bid
        if bid not in bm.bins:
            bm.bins[bid] = []
        bm.bins[bid].append((theta, float(fitness)))
    bm._rebuild()

    loaded_history = np.load(path + 'history.npy', allow_pickle=True).item()
    for k in cmame.history:
        if k in loaded_history:
            cmame.history[k] = loaded_history[k]

    cmame.total_evals = int(np.load(path + 'total_evals.npy'))

    # Reset emitters — they will reinitialize from archive
    for emitter in cmame.emitters:
        emitter.es = None
