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



class PlanarArmBD_CVT:
    """
    CVT-based behavior descriptor for planar arm.
    discretize() assigns BD to nearest center.

    Args:
        n_bins: number of bins
        centers: center initialization method
            - "cvt": compute CVT via Lloyd's algorithm
            - "random": random sample inside unit circle
            - np.ndarray of shape (n_bins, 2): use directly
        radius: radius of reachable region
        cvt_iters: Lloyd's algorithm iterations (only if centers="cvt")
        cvt_samples: samples per iteration (only if centers="cvt")
        seed: random seed for center computation
    """

    def __init__(self, n_bins=1950, centers="random", radius=1.0,
                 cvt_iters=100, cvt_samples=100000, seed=0):
        self.n_bins = n_bins
        self.radius = radius

        if isinstance(centers, np.ndarray):
            self.centers = centers
            self.n_bins = len(centers)
        elif centers == "cvt":
            self.centers = self._compute_cvt(n_bins, radius, cvt_iters, cvt_samples, seed)
        elif centers == "random":
            rng = np.random.RandomState(seed)
            self.centers = self._sample_unit_circle(n_bins, radius, rng)
        elif centers == "Precomputed_CVT_1950":
            self.centers = self._precomp1950()
            self.n_bins = len(self.centers)
        else:
            raise ValueError(f"Unknown centers option: {centers}")

    def _sample_unit_circle(self, n, radius, rng):
        """Sample n points uniformly inside a circle of given radius."""
        angles = rng.uniform(0, 2 * np.pi, n)
        radii = radius * np.sqrt(rng.uniform(0, 1, n))
        x = radii * np.cos(angles)
        y = radii * np.sin(angles)
        return np.column_stack([x, y])

    def _compute_cvt(self, n_bins, radius, cvt_iters, cvt_samples, seed):
        """Compute CVT centers via Lloyd's algorithm."""
        rng = np.random.RandomState(seed)
        centers = self._sample_unit_circle(n_bins, radius, rng)

        for _ in range(cvt_iters):
            samples = self._sample_unit_circle(cvt_samples, radius, rng)
            diffs = samples[:, None, :] - centers[None, :, :]
            dists = np.sum(diffs ** 2, axis=2)
            assignments = np.argmin(dists, axis=1)
            new_centers = np.empty_like(centers)
            for i in range(n_bins):
                mask = assignments == i
                if mask.any():
                    new_centers[i] = samples[mask].mean(axis=0)
                else:
                    new_centers[i] = centers[i]
            centers = new_centers

        return centers

    def describe(self, info):
        """
        Args:
            info: dict from PlanarArmCollector.collect()

        Returns:
            (x, y) end-effector position
        """
        return info['end_effector']

    def discretize(self, descriptor):
        """
        Assign descriptor to nearest center.

        Returns:
            int bin index
        """
        point = np.array(descriptor)
        dists = np.sum((self.centers - point) ** 2, axis=1)
        return int(np.argmin(dists))

    def total_bins(self):
        return self.n_bins

    def _precomp1950(self):
        """Precomputed CVT centers (1950 bins, radius=1.0, seed=0, 100 iters, 100k samples)."""
        return np.array([
            # PASTE ARRAY HERE
        ])
