from abc import ABC, abstractmethod
import numpy as np



class BehaviorDescriptor(ABC):
    """
    Abstract class for behavior descriptor.
    Extracts behavior from raw info dict produced by ProblemSetting.collect().
    """

    @abstractmethod
    def describe(self, info):
        """
        Compute behavior descriptor from collected info.

        Args:
            info: dict of lists from ProblemSetting.collect()

        Returns:
            behavior descriptor (e.g. tuple or array)
        """
        pass



class BipedalWalkerBD_v1:
    """
    3D behavior descriptor for BipedalWalker:
    (leg1_contact_ratio, leg2_contact_ratio, hull_mean_angle)

    Args:
        bin_ranges: list of (min, max) per dimension
        bin_sizes: list of number of bins per dimension
    """

    def __init__(self, bin_ranges=None, bin_sizes=None):
        if bin_ranges is None:
            bin_ranges = [(0.0, 1.0), (0.0, 1.0), (-1.5, 1.5)]
        if bin_sizes is None:
            bin_sizes = [10, 10, 10]
        self.bin_ranges = bin_ranges
        self.bin_sizes = bin_sizes

    def describe(self, info):
        """
        Compute behavior descriptor from collected info.

        Args:
            info: dict from BipedalWalkerCollector.collect()

        Returns:
            (leg1_ratio, leg2_ratio, hull_mean_angle)
        """
        all_leg1 = np.concatenate(info['leg1_contacts'])
        all_leg2 = np.concatenate(info['leg2_contacts'])
        all_angles = np.concatenate(info['hull_angles'])

        leg1_ratio = float(all_leg1.mean())
        leg2_ratio = float(all_leg2.mean())
        hull_mean_angle = float(all_angles.mean())

        return (leg1_ratio, leg2_ratio, hull_mean_angle)

    def discretize(self, descriptor):
        """
        Map continuous descriptor to grid bin ID.

        Args:
            descriptor: tuple from describe()

        Returns:
            tuple of int bin indices (clamped to valid range)
        """
        bin_id = []
        for val, (lo, hi), n_bins in zip(descriptor, self.bin_ranges, self.bin_sizes):
            clamped = np.clip(val, lo, hi)
            idx = int((clamped - lo) / (hi - lo) * n_bins)
            idx = min(idx, n_bins - 1)
            bin_id.append(idx)
        return tuple(bin_id)

    def total_bins(self):
        """Total number of bins in the grid."""
        result = 1
        for n in self.bin_sizes:
            result *= n
        return result

    def bin_value(self, bin_id):
        """Return grid cell center for given bin_id tuple."""
        value = []
        for idx, (lo, hi), n_bins in zip(bin_id, self.bin_ranges, self.bin_sizes):
            cell_width = (hi - lo) / n_bins
            value.append(lo + (idx + 0.5) * cell_width)
        return np.array(value)



class CartPoleBD_v1:
    """
    2D behavior descriptor for CartPole:
    (mean_cart_position, action_switch_rate)

    Args:
        bin_ranges: list of (min, max) per dimension
        bin_sizes: list of number of bins per dimension
    """

    def __init__(self, bin_ranges=None, bin_sizes=None):
        if bin_ranges is None:
            bin_ranges = [(-2.4, 2.4), (0.0, 1.0)]
        if bin_sizes is None:
            bin_sizes = [20, 20]
        self.bin_ranges = bin_ranges
        self.bin_sizes = bin_sizes

    def describe(self, info):
        """
        Args:
            info: dict from CartPoleCollector.collect()

        Returns:
            (mean_cart_position, action_switch_rate)
        """
        all_positions = np.concatenate(info['cart_positions'])
        all_actions = np.concatenate(info['actions'])

        mean_pos = float(all_positions.mean())

        if len(all_actions) < 2:
            switch_rate = 0.0
        else:
            switches = np.sum(all_actions[1:] != all_actions[:-1])
            switch_rate = float(switches / (len(all_actions) - 1))

        return (mean_pos, switch_rate)

    def discretize(self, descriptor):
        bin_id = []
        for val, (lo, hi), n_bins in zip(descriptor, self.bin_ranges, self.bin_sizes):
            clamped = np.clip(val, lo, hi)
            idx = int((clamped - lo) / (hi - lo) * n_bins)
            idx = min(idx, n_bins - 1)
            bin_id.append(idx)
        return tuple(bin_id)

    def total_bins(self):
        result = 1
        for n in self.bin_sizes:
            result *= n
        return result
        
    def bin_value(self, bin_id):
        """Return grid cell center for given bin_id tuple."""
        value = []
        for idx, (lo, hi), n_bins in zip(bin_id, self.bin_ranges, self.bin_sizes):
            cell_width = (hi - lo) / n_bins
            value.append(lo + (idx + 0.5) * cell_width)
        return np.array(value)
