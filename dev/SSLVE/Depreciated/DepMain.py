import matplotlib.pyplot as plt



class MAPElite:
    """
    MAP-Elites without latent module.

    Orchestrates: SearchPhase -> Collector -> BehaviorMatching

    Args:
        search_phase (SP): generates candidate thetas, converts to agents
        collector (CO): collects raw episode info from agents
        behavior_matching (BM): manages archive with behavior descriptors
    """

    def __init__(self, search_phase, collector, behavior_matching):
        self.SP = search_phase
        self.CO = collector
        self.BM = behavior_matching
        self.history = {
            'fitness_min': [],
            'fitness_mean': [],
            'fitness_max': [],
            'coverage': [],
            'archive_size': [],
        }

    def step(self):
        """
        One MAP-Elite iteration:
        1. SP generates thetas
        2. SP converts to agents, CO collects info
        3. BM updates archive
        """
        thetas = self.SP.sample(
            collector=self.CO,
            behavior_matching=self.BM,
        )

        import time
        infos = []
        n_total = len(thetas)
        t_start = time.time()
        for i, theta in enumerate(thetas):
            agent = self.SP.make_agent(theta)
            info = self.CO.collect(agent)
            infos.append(info)
            elapsed = time.time() - t_start
            rate = (i + 1) / elapsed if elapsed > 0 else 0
            eta = (n_total - i - 1) / rate if rate > 0 else 0
            print(f"\rCollecting: {i+1}/{n_total} [{elapsed:.0f}s elapsed, {eta:.0f}s remaining]", end="", flush=True)
        print()

        self.BM.update(thetas, infos)

        f_min, f_mean, f_max = self.BM.fitness_stats()
        self.history['fitness_min'].append(f_min)
        self.history['fitness_mean'].append(f_mean)
        self.history['fitness_max'].append(f_max)
        self.history['coverage'].append(self.BM.coverage())
        self.history['archive_size'].append(self.BM.archive_size())

        print(f"Archive: {self.BM.archive_size()}, "
              f"Bins: {len(self.BM.bins)}, "
              f"Coverage: {self.BM.coverage():.4f}, "
              f"Fitness min/mean/max: {f_min:.2f}/{f_mean:.2f}/{f_max:.2f}")

    def run(self, n_steps):
        """
        Full MAP-Elite loop.

        Args:
            n_steps: number of iterations
        """
        for t in range(n_steps):
            print(f"\n--- MAP-Elite Step {t+1}/{n_steps} ---")
            self.step()

    def plot_history(self, save_path=None):
        if not self.history['fitness_min']:
            print("No history available.")
            return

        steps = range(len(self.history['fitness_min']))
        fig, axes = plt.subplots(1, 3, figsize=(18, 5))

        ax = axes[0]
        ax.plot(steps, self.history['fitness_min'], label='Min')
        ax.plot(steps, self.history['fitness_mean'], label='Mean')
        ax.plot(steps, self.history['fitness_max'], label='Max')
        ax.set_xlabel('Step')
        ax.set_ylabel('Fitness')
        ax.set_title('Fitness over Steps')
        ax.legend()

        ax = axes[1]
        ax.plot(steps, self.history['coverage'])
        ax.set_xlabel('Step')
        ax.set_ylabel('Coverage')
        ax.set_title('Behavior Coverage')

        ax = axes[2]
        ax.plot(steps, self.history['archive_size'])
        ax.set_xlabel('Step')
        ax.set_ylabel('Size')
        ax.set_title('Archive Size')

        plt.tight_layout()
        if save_path:
            plt.savefig(save_path, bbox_inches='tight')
        else:
            plt.show()
        plt.close()
