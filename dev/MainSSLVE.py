class SSLVE:
    """
    Self-Supervised Latent Variable Evolution.

    Iterates between:
      1. search_and_update: run search phase, update bins with new samples
      2. train_latent: train BetaVAE_SSLVE with current dataset and bin structure

    Args:
        latent_module: BetaVAE_SSLVE instance
        search_phase: SearchPhase instance
        behavior_matching: BehaviorMatching instance
        device: 'cpu' or 'cuda'
    """

    def __init__(self, latent_module, search_phase, behavior_matching, device='cpu'):
        self.latent_module = latent_module
        self.search_phase = search_phase
        self.behavior_matching = behavior_matching
        self.device = device

    def initialize(self):
        """
        Phase t=0: generate initial dataset and build initial bins.
        """
        initial_thetas = self.search_phase.sample(behavior_matching=None)
        self.behavior_matching.update_bins(initial_thetas)

    def search_and_update(self):
        """
        Search phase: generate new samples using current bins,
        then update bins and dataset.
        """
        new_thetas = self.search_phase.sample(behavior_matching=self.behavior_matching)
        self.behavior_matching.update_bins(new_thetas)

    def train_latent(self, **kwargs):
        """
        Train latent module on current dataset with current bin structure.

        Args:
            **kwargs: passed to latent_module.fit()

        Returns:
            loss history dict
        """
        return self.latent_module.fit(
            dataset=self.behavior_matching.dataset,
            bin_ids=self.behavior_matching.bin_ids,
            bins=self.behavior_matching.bins,
            device=self.device,
            **kwargs
        )

    def run(self, n_phases, **train_kwargs):
        """
        Full SSLVE loop.

        Args:
            n_phases: number of search+train iterations after initialization
            **train_kwargs: passed to train_latent each phase

        Returns:
            list of loss history dicts, one per phase
        """
        self.initialize()
        histories = []
        for t in range(n_phases):
            print(f"\n--- SSLVE Phase {t+1}/{n_phases} ---")
            print(f"Dataset size: {len(self.behavior_matching.dataset)}, "
                  f"Bins: {len(self.behavior_matching.bins)}")
            history = self.train_latent(**train_kwargs)
            histories.append(history)
            if t < n_phases - 1:
                self.search_and_update()
        return histories
