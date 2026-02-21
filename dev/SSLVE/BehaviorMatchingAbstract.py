from abc import ABC, abstractmethod


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
