class SSLVE:
    """
    Self-Supervised Latent Variable Evolution.

    Orchestrates: SearchPhase -> Collector -> BehaviorMatching -> LatentModule

    Args:
        search_phase (SP): generates candidate thetas, converts to agents
        collector (CO): collects raw episode info from agents
        behavior_matching (BM): manages archive with behavior descriptors
        latent_module (LM): trains representation on archive
        device: 'cpu' or 'cuda'
    """

    def __init__(self, search_phase, collector, behavior_matching, latent_module, device='cpu'):
        self.SP = search_phase
        self.CO = collector
        self.BM = behavior_matching
        self.LM = latent_module
        self.device = device

    def step(self, train_kwargs=None):
        """
        One SSLVE iteration:
        1. SP generates thetas
        2. SP converts to agents, PS collects info
        3. BM updates archive
        4. LM trains on archive

        Args:
            train_kwargs: dict passed to LM.fit()

        Returns:
            loss history from LM.fit()
        """
        if train_kwargs is None:
            train_kwargs = {}

        # Search
        thetas = self.SP.sample(latent_module=self.LM, behavior_matching=self.BM)

        # Collect
        infos = []
        for theta in thetas:
            agent = self.SP.make_agent(theta)
            info = self.CO.collect(agent)
            infos.append(info)

        # Update archive
        self.BM.update(thetas, infos)

        # Train latent module
        history = self.LM.fit(
            dataset=self.BM.dataset,
            bin_ids=self.BM.bin_ids,
            bins=self.BM.bins,
            device=self.device,
            **train_kwargs
        )
        return history

    def run(self, n_steps, train_kwargs=None):
        """
        Full SSLVE loop.

        Args:
            n_steps: number of iterations
            train_kwargs: dict passed to LM.fit() each step

        Returns:
            list of loss histories
        """
        histories = []
        for t in range(n_steps):
            print(f"\n--- SSLVE Step {t+1}/{n_steps} ---")
            print(f"Archive size: {len(self.BM.dataset)}, Bins: {len(self.BM.bins)}")
            history = self.step(train_kwargs)
            histories.append(history)
        return histories
