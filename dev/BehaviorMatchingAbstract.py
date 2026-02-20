from abc import ABC, abstractmethod


class BehaviorMatching(ABC):
    """
    Abstract class for behavior-matching bin management.

    Maintains:
        dataset  : list of numpy arrays (all stored genes)
        bin_ids  : list of bin IDs, bin_ids[i] is bin of dataset[i]
        bins     : dict {bin_id: [dataset_indices]}

    Args:
        behavior_descriptor: BehaviorDescriptor instance
    """

    def __init__(self, behavior_descriptor):
        self.behavior_descriptor = behavior_descriptor
        self.dataset = []
        self.bin_ids = []
        self.bins = {}

    @abstractmethod
    def update_bins(self, new_thetas):
        """
        Update bins with new genes.

        At t=0 (empty dataset): initializes bins from scratch.
        At t>0: updates existing bins with new_thetas.

        Args:
            new_thetas: list of numpy arrays

        Updates self.dataset, self.bin_ids, self.bins in place.
        """
        pass
