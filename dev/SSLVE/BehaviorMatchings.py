from abc import ABC, abstractmethod
import numpy as np


class BehaviorMatching(ABC):
    """
    Abstract class for behavior-matching archive management.

    Maintains:
        dataset   : list of numpy arrays (stored thetas)
        fitnesses : list of floats
        bin_ids   : list of bin IDs, bin_ids[i] is bin of dataset[i]
        bins      : dict {bin_id: [dataset_indices]}

    Args:
        behavior_descriptor: BehaviorDescriptor instance
        fitness_fn: callable(info_dict) -> float (scalar fitness, to minimize)
    """

    def __init__(self, behavior_descriptor, fitness_fn):
        self.behavior_descriptor = behavior_descriptor
        self.fitness_fn = fitness_fn
        self.dataset = []
        self.fitnesses = []
        self.bin_ids = []
        self.bins = {}

    @abstractmethod
    def update(self, thetas, infos):
        """
        Update archive with new candidates.

        Args:
            thetas: list of numpy arrays
            infos: list of info dicts from ProblemSetting.collect()

        Updates self.dataset, self.fitnesses, self.bin_ids, self.bins in place.
        """
        pass



class MAPElitesBM:
    """
    MAP-Elites style behavior matching with top-k per bin.

    Maintains an archive of thetas organized by behavior bins.
    Each bin keeps at most top_k entries ranked by fitness.

    Args:
        behavior_descriptor: object with describe(info) and discretize(descriptor)
        fitness_fn: callable(info) -> float (to minimize)
        top_k: max entries per bin
    """

    def __init__(self, behavior_descriptor, fitness_fn, top_k=10):
        self.behavior_descriptor = behavior_descriptor
        self.fitness_fn = fitness_fn
        self.top_k = top_k
        self.dataset = []
        self.fitnesses = []
        self.bin_ids = []
        self.bins = {}  # {bin_id: [dataset_indices]}

    def update(self, thetas, infos):
        """
        Update archive with new candidates.

        Args:
            thetas: list of numpy arrays
            infos: list of info dicts from Collector.collect()
        """
        for theta, info in zip(thetas, infos):
            descriptor = self.behavior_descriptor.describe(info)
            bin_id = self.behavior_descriptor.discretize(descriptor)
            fitness = self.fitness_fn(info)

            idx = len(self.dataset)
            self.dataset.append(theta)
            self.fitnesses.append(fitness)
            self.bin_ids.append(bin_id)

            if bin_id not in self.bins:
                self.bins[bin_id] = []
            self.bins[bin_id].append(idx)

            # Keep top-k per bin (lowest fitness = best, since minimizing)
            if len(self.bins[bin_id]) > self.top_k:
                bin_indices = self.bins[bin_id]
                bin_fitnesses = [self.fitnesses[i] for i in bin_indices]
                worst_pos = int(np.argmax(bin_fitnesses))
                worst_idx = bin_indices[worst_pos]
                bin_indices.pop(worst_pos)

    def coverage(self):
        """Fraction of occupied bins over total possible bins."""
        total = self.behavior_descriptor.total_bins()
        return len(self.bins) / total

    def archive_size(self):
        """Total number of entries across all bins."""
        return sum(len(v) for v in self.bins.values())

    def fitness_stats(self):
        """Return min, mean, max fitness across active archive entries."""
        active_indices = []
        for indices in self.bins.values():
            active_indices.extend(indices)
        if not active_indices:
            return 0.0, 0.0, 0.0
        active_fitnesses = [self.fitnesses[i] for i in active_indices]
        return float(np.min(active_fitnesses)), float(np.mean(active_fitnesses)), float(np.max(active_fitnesses))
