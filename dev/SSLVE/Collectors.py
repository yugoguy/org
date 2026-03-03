from abc import ABC, abstractmethod
import numpy as np
import gymnasium as gym


class Collector(ABC):
    """
    Abstract class for environment data collection.
    Collects raw episode data from an agent.
    """

    @abstractmethod
    def collect(self, agent):
        """
        Run agent in environment and collect raw info.

        Args:
            agent: agent with act(obs) method, weights already set

        Returns:
            dict of lists, each list has one entry per episode.
            e.g. {'reward': [r1, r2], 'final_x': [x1, x2], ...}
        """
        pass



class BipedalWalkerCollector:
    """
    Collector for BipedalWalker-v3.
    Runs episodes and returns raw per-episode data.

    Args:
        max_steps: max steps per episode
        n_episodes: number of episodes
        seed: random seed
    """

    def __init__(self, max_steps=1600, n_episodes=3, seed=None):
        self.max_steps = max_steps
        self.n_episodes = n_episodes
        self.seed = seed

    def collect(self, agent):
        """
        Run agent and collect raw info.

        Returns:
            dict with keys:
                'reward': list of total reward per episode
                'steps': list of steps survived per episode
                'leg1_contacts': list of arrays of leg1 contact per timestep
                'leg2_contacts': list of arrays of leg2 contact per timestep
                'hull_angles': list of arrays of hull angle per timestep
                'final_x': list of final hull x position per episode
        """
        info = {
            'reward': [],
            'steps': [],
            'leg1_contacts': [],
            'leg2_contacts': [],
            'hull_angles': [],
            'final_x': [],
        }

        for ep in range(self.n_episodes):
            env = gym.make('BipedalWalker-v3')
            seed = self.seed + ep if self.seed is not None else None
            obs, _ = env.reset(seed=seed)
            ep_reward = 0
            leg1 = []
            leg2 = []
            angles = []

            for _ in range(self.max_steps):
                action = agent.act(obs)
                obs, reward, terminated, truncated, _ = env.step(action)
                ep_reward += reward
                leg1.append(obs[8])
                leg2.append(obs[13])
                angles.append(obs[0])
                if terminated or truncated:
                    break

            info['reward'].append(ep_reward)
            info['steps'].append(len(leg1))
            info['leg1_contacts'].append(np.array(leg1))
            info['leg2_contacts'].append(np.array(leg2))
            info['hull_angles'].append(np.array(angles))
            info['final_x'].append(env.unwrapped.hull.position.x)
            env.close()

        return info



class CartPoleCollector:
    """
    Collector for CartPole-v1.

    Args:
        max_steps: max steps per episode
        n_episodes: number of episodes
        seed: random seed
    """

    def __init__(self, max_steps=500, n_episodes=3, seed=None):
        self.max_steps = max_steps
        self.n_episodes = n_episodes
        self.seed = seed

    def collect(self, agent):
        """
        Returns:
            dict with keys:
                'reward': list of total reward per episode
                'steps': list of steps per episode
                'cart_positions': list of arrays of cart position per timestep
                'actions': list of arrays of actions per timestep
        """
        import gymnasium as gym
        import numpy as np

        info = {
            'reward': [],
            'steps': [],
            'cart_positions': [],
            'actions': [],
        }

        for ep in range(self.n_episodes):
            env = gym.make('CartPole-v1')
            seed = self.seed + ep if self.seed is not None else None
            obs, _ = env.reset(seed=seed)
            ep_reward = 0
            positions = []
            actions = []

            for _ in range(self.max_steps):
                action = agent.act(obs)
                obs, reward, terminated, truncated, _ = env.step(action)
                ep_reward += reward
                positions.append(obs[0])
                actions.append(action)
                if terminated or truncated:
                    break

            info['reward'].append(ep_reward)
            info['steps'].append(len(positions))
            info['cart_positions'].append(np.array(positions))
            info['actions'].append(np.array(actions))
            env.close()

        return info



class PlanarArmCollector:
    """
    Collector for planar arm inverse kinematics, generalized to N-dim.

    N-dim FK uses spherical coordinates: each joint has (end_effector_dim - 1)
    angles. For 2D this is 1 angle per joint (standard planar arm).

    Args:
        n_joints: number of joints
        end_effector_dim: dimension of end-effector space (default 2)
        link_length: length of each link (default: 1/n_joints)
        noise_sigma: std of Gaussian noise added to angles per episode (default 0)
        n_episodes: number of noised episodes to average (default 1, ignored if noise_sigma=0)
    """

    def __init__(self, n_joints, end_effector_dim=2, link_length=None,
                 noise_sigma=0.0, n_episodes=1):
        self.n_joints = n_joints
        self.end_effector_dim = end_effector_dim
        self.link_length = link_length if link_length is not None else 1.0 / n_joints
        self.noise_sigma = noise_sigma
        self.n_episodes = n_episodes
        self.angles_per_joint = end_effector_dim - 1

    def _spherical_to_cartesian(self, angles_per_joint_array):
        """
        Convert (n_joints, angles_per_joint) spherical angles to unit direction vectors.
        For 2D: angles shape (n_joints, 1), output (n_joints, 2).
        For 3D: angles shape (n_joints, 2), output (n_joints, 3).
        General N-dim: angles shape (n_joints, N-1), output (n_joints, N).

        Uses cumulative angles per component, then standard spherical-to-cartesian.
        """
        n_joints = angles_per_joint_array.shape[0]
        dim = self.end_effector_dim
        k = self.angles_per_joint  # dim - 1

        # Cumulative angles per component
        cum_angles = np.cumsum(angles_per_joint_array, axis=0)  # (n_joints, k)

        directions = np.ones((n_joints, dim))
        for i in range(k):
            # dimensions 0..i get sin(angle_i) factor
            # dimension i+1..dim-1 keep cos(angle_i) factor at step i
            # Standard spherical: x_0 = cos(a0), x_1 = sin(a0)cos(a1), ..., x_{k} = sin(a0)...sin(a_{k-1})
            pass

        # Spherical to cartesian (cumulative angles already applied)
        # For dim=2: direction = (cos(cum_a0), sin(cum_a0))
        # For dim=N: standard conversion
        for j in range(n_joints):
            a = cum_angles[j]  # (k,) angles
            v = np.ones(dim)
            for i in range(k):
                v[i] *= np.cos(a[i])
                for d in range(i + 1, dim):
                    v[d] *= np.sin(a[i])
            directions[j] = v

        return directions

    def _fk(self, angles_flat):
        """
        Forward kinematics from flat angle vector.

        Args:
            angles_flat: 1D array of length n_joints * angles_per_joint

        Returns:
            end_effector: tuple of floats, length end_effector_dim
        """
        angles = angles_flat.reshape(self.n_joints, self.angles_per_joint)
        directions = self._spherical_to_cartesian(angles)
        position = np.sum(self.link_length * directions, axis=0)
        return tuple(float(x) for x in position)

    def _compute_metrics(self, angles_flat):
        k = self.angles_per_joint
        angles = angles_flat.reshape(self.n_joints, k)
    
        if k == 1:
            angles_1d = angles[:, 0]
            variance = float(np.var(angles_1d))
            local_dep = float(np.sum(np.abs(np.diff(angles_1d))))
            sine_dep = float(np.mean((angles_1d[:-1] - np.pi * np.sin(angles_1d[1:])) ** 2))
            return variance, local_dep, sine_dep
        else:
            variances = [float(np.var(angles[:, c])) for c in range(k)]
            local_deps = [float(np.sum(np.abs(np.diff(angles[:, c])))) for c in range(k)]
            sine_deps = [float(np.mean((angles[:-1, c] - np.pi * np.sin(angles[1:, c])) ** 2)) for c in range(k)]
            return variances, local_deps, sine_deps

    def collect(self, agent):
        """
        Compute FK from agent's joint angles, optionally with noise.

        Returns:
            dict with keys:
                'joint_angles': numpy array of angles (original, no noise)
                'end_effector': tuple (mean over episodes if noised)
                'angle_variance': float (2D) or list of floats (>2D)
                'local_abs_dependency': float (2D) or list of floats (>2D)
        """
        angles = agent.angles
        n_ep = 1 if self.noise_sigma == 0.0 else self.n_episodes

        end_effectors = []
        for _ in range(n_ep):
            if self.noise_sigma > 0.0:
                noised = angles + self.noise_sigma * np.random.randn(len(angles))
            else:
                noised = angles
            ee = self._fk(noised)
            end_effectors.append(ee)

        mean_ee = tuple(float(np.mean([ee[d] for ee in end_effectors]))
                        for d in range(self.end_effector_dim))

        angle_variance, local_abs_dependency = self._compute_metrics(angles)

        return {
            'joint_angles': angles,
            'end_effector': mean_ee,
            'angle_variance': angle_variance,
            'local_abs_dependency': local_abs_dependency,
        }
