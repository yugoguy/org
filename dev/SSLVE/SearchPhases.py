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



class BoltzmannMix:
    """
    Adaptive search phase using Boltzmann (softmax) exploration over
    arbitrary variation operators.

    Warmup phase: uses warmup_operator (default: first operator) until
    archive reaches warmup_threshold.
    Adaptive phase: allocate n_total samples across operators using softmax
    over EMA of per-operator mean reward.

    Args:
        agent_class: class with architecture info
        architecture: architecture spec (list or int)
        operators: list of VariationOperator instances
        n_total: total samples per step
        warmup_threshold: archive size before adaptive mixing starts
        ema_alpha: EMA decay rate for reward tracking
        temperature: softmax temperature (higher = more uniform)
        min_proportion: minimum fraction of n_total per operator
        init_fn: callable() -> numpy array, custom init (default: He-init)
        warmup_operator: VariationOperator for warmup (default: operators[0])
        agent_kwargs: dict for agent constructor
    """

    def __init__(self, agent_class, architecture, operators,
                 n_total=200, warmup_threshold=512,
                 ema_alpha=0.3, temperature=1.0, min_proportion=0.05,
                 init_fn=None, warmup_operator=None, agent_kwargs=None):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}
        self.operators = operators
        self.n_total = n_total
        self.warmup_threshold = warmup_threshold
        self.ema_alpha = ema_alpha
        self.temperature = temperature
        self.min_proportion = min_proportion
        self.init_fn = init_fn
        self.warmup_operator = warmup_operator or operators[0]

        n_ops = len(operators)
        self.ema_rates = np.ones(n_ops)
        self.prev_tags = None
        self.warmed_up = False

        self.allocation_history = []
        self.ema_history = []

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

    def _softmax_allocation(self):
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

        return counts

    def _update_ema(self, rewards, tags):
        op_names = [op.name for op in self.operators]
        for i, name in enumerate(op_names):
            op_rewards = [r for r, t in zip(rewards, tags) if t == name]
            if len(op_rewards) > 0:
                mean_reward = np.mean(op_rewards)
                self.ema_rates[i] = (self.ema_alpha * mean_reward
                                     + (1 - self.ema_alpha) * self.ema_rates[i])

    def _record_allocation(self, counts):
        self.allocation_history.append(tuple(counts))
        self.ema_history.append(tuple(self.ema_rates.copy()))
        total = sum(counts)
        parts = [f"{op.name}: {c}/{total} ({c/total:.0%})"
                 for op, c in zip(self.operators, counts)]
        print(f"  Operator ratio - {', '.join(parts)}")

    def sample(self, latent_module=None, behavior_matching=None, **kwargs):
        bm = behavior_matching

        # Process rewards from previous step
        if self.prev_tags is not None and bm is not None and bm.rewards is not None:
            self._update_ema(bm.rewards, self.prev_tags)

        # No archive: random init
        if bm is None or len(bm.bins) == 0:
            self.prev_tags = None
            if bm is not None:
                bm.compute_rewards = False
            if latent_module is not None:
                latent_module.skip_training = True
            self._record_allocation([self.n_total] + [0] * (len(self.operators) - 1))
            return [self._init() for _ in range(self.n_total)]

        # Warmup phase
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
                candidates = self.warmup_operator(bm, self.n_total, latent_module)
                tags = [self.warmup_operator.name] * len(candidates)
                self.prev_tags = tags
                counts = [0] * len(self.operators)
                wi = next((i for i, op in enumerate(self.operators)
                           if op is self.warmup_operator), 0)
                counts[wi] = self.n_total
                self._record_allocation(counts)
                return candidates

        # Adaptive phase
        counts = self._softmax_allocation()
        self._record_allocation(counts)

        candidates = []
        tags = []

        for op, c in zip(self.operators, counts):
            children = op(bm, c, latent_module)
            candidates.extend(children)
            tags.extend([op.name] * len(children))

        self.prev_tags = tags
        return candidates

    def plot_allocation(self, save_path=None):
        if not self.allocation_history:
            print("No allocation history to plot.")
            return

        history = np.array(self.allocation_history, dtype=float)
        totals = history.sum(axis=1, keepdims=True)
        totals = np.maximum(totals, 1)
        ratios = history / totals

        steps = np.arange(len(ratios))
        n_ops = len(self.operators)

        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8), sharex=True)

        labels = [op.name for op in self.operators]
        ax1.stackplot(steps, *[ratios[:, i] for i in range(n_ops)],
                      labels=labels, alpha=0.8)
        ax1.set_ylabel('Proportion')
        ax1.set_title('Operator Allocation Over Steps')
        ax1.legend(loc='upper right')
        ax1.set_ylim(0, 1)

        if self.ema_history:
            ema = np.array(self.ema_history)
            for i in range(n_ops):
                ax2.plot(steps, ema[:, i], label=labels[i])
            ax2.set_ylabel('EMA Reward Rate')
            ax2.set_title('EMA Reward Rates Over Steps')
            ax2.legend(loc='upper right')

        ax2.set_xlabel('Step')
        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=150, bbox_inches='tight')
        plt.show()
