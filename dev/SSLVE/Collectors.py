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
            cosine_dep = float(np.mean((angles_1d[:-1] - np.pi * np.cos(angles_1d[1:])) ** 2))
            return variance, local_dep, sine_dep, cosine_dep
        else:
            variances = [float(np.var(angles[:, c])) for c in range(k)]
            local_deps = [float(np.sum(np.abs(np.diff(angles[:, c])))) for c in range(k)]
            sine_deps = [float(np.mean((angles[:-1, c] - np.pi * np.sin(angles[1:, c])) ** 2)) for c in range(k)]
            cosine_deps = [float(np.mean((angles[:-1, c] - np.pi * np.cos(angles[1:, c])) ** 2)) for c in range(k)]
            return variances, local_deps, sine_deps, cosine_deps

    def collect(self, agent):
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

        angle_variance, local_abs_dependency, sine_dependency, cosine_dependency = self._compute_metrics(angles)

        return {
            'joint_angles': angles,
            'end_effector': mean_ee,
            'angle_variance': angle_variance,
            'local_abs_dependency': local_abs_dependency,
            'sine_dependency': sine_dependency,
            'cosine_dependency': cosine_dependency,
        }



class AntOmniCollector:
    """
    Collector for Ant-v5 omni-directional locomotion.
    Runs episodes and returns final CoM (x, y), survival sum, torque sum,
    cumulative path length, and heading angle variance (curvature consistency).

    Args:
        max_steps: max steps per episode
        n_episodes: number of episodes to average
        ctrl_cost_weight: weight for control cost in env reward
        seed: random seed
    """

    def __init__(self, max_steps=1000, n_episodes=1, ctrl_cost_weight=0.5, seed=None):
        self.max_steps = max_steps
        self.n_episodes = n_episodes
        self.ctrl_cost_weight = ctrl_cost_weight
        self.seed = seed

    def collect(self, agent):
        final_xs = []
        final_ys = []
        survival_sums = []
        torque_sums = []
        steps_list = []
        path_lengths = []
        heading_angle_vars = []

        for ep in range(self.n_episodes):
            env = gym.make('Ant-v5',
                           ctrl_cost_weight=self.ctrl_cost_weight,
                           healthy_reward=1.0,
                           terminate_when_unhealthy=True,
                           include_cfrc_ext_in_observation=False,
                           max_episode_steps=self.max_steps)
            seed = self.seed + ep if self.seed is not None else None
            obs, _ = env.reset(seed=seed)
            survival_total = 0.0
            torque_total = 0.0
            n_steps = 0
            path_length = 0.0
            prev_xy = env.unwrapped.get_body_com("torso")[:2].copy()
            headings = []

            for _ in range(self.max_steps):
                action = agent.act(obs)
                obs, reward, terminated, truncated, step_info = env.step(action)
                survival_total += step_info.get('reward_survive', 1.0)
                torque_total += step_info.get('reward_ctrl', 0.0)
                n_steps += 1

                xy = env.unwrapped.get_body_com("torso")[:2]
                delta = xy - prev_xy
                dist = float(np.linalg.norm(delta))
                path_length += dist
                if dist > 1e-8:
                    headings.append(np.arctan2(delta[1], delta[0]))
                prev_xy = xy.copy()

                if terminated or truncated:
                    break

            # Variance of turning angles (heading differences, wraparound-corrected)
            if len(headings) >= 2:
                headings = np.array(headings)
                diffs = np.diff(headings)
                diffs = (diffs + np.pi) % (2 * np.pi) - np.pi
                heading_angle_vars.append(float(np.var(diffs)))
            else:
                heading_angle_vars.append(0.0)

            final_xs.append(float(xy[0]))
            final_ys.append(float(xy[1]))
            survival_sums.append(survival_total)
            torque_sums.append(abs(torque_total))
            steps_list.append(n_steps)
            path_lengths.append(path_length)
            env.close()

        return {
            'final_xy': (float(np.mean(final_xs)), float(np.mean(final_ys))),
            'survival_sum': float(np.mean(survival_sums)),
            'torque_sum': float(np.mean(torque_sums)),
            'steps': float(np.mean(steps_list)),
            'path_length': float(np.mean(path_lengths)),
            'heading_angle_var': float(np.mean(heading_angle_vars)),
        }



class PointMassCollector:
    """
    Collector for 2D point mass trajectory generation.

    Dynamics:
        vel = friction * vel + force
        pos = pos + dt * vel

    Agent input: (pos_x, pos_y, vel_x, vel_y, t/T)  — 5D
    Agent output: (force_x, force_y) — 2D, tanh-bounded [-1, 1]

    Reachable radius = n_steps * dt / (1 - friction).
    Tune params so this equals desired radius (e.g. 1.0).

    Args:
        friction: velocity decay factor per step (0 < friction < 1)
        dt: time step size
        n_steps: number of simulation steps per episode
        noise_sigma: std of Gaussian noise added to force per episode (0 = deterministic)
        n_episodes: number of noised episodes to average (ignored if noise_sigma=0)
    """

    def __init__(self, friction=0.9, dt=0.01, n_steps=100,
                 noise_sigma=0.0, n_episodes=1):
        self.friction = friction
        self.dt = dt
        self.n_steps = n_steps
        self.noise_sigma = noise_sigma
        self.n_episodes = n_episodes
        self.max_path_length = n_steps * dt / (1.0 - friction)

    def _simulate(self, agent):
        """Run one episode, return final_xy, heading_angle_var, path_length."""
        pos = np.array([0.0, 0.0])
        vel = np.array([0.0, 0.0])
        T = self.n_steps

        headings = []
        path_length = 0.0

        for t in range(1, T + 1):
            obs = np.array([pos[0], pos[1], vel[0], vel[1], t / T])
            force = agent.act(obs)

            if self.noise_sigma > 0.0:
                force = force + self.noise_sigma * np.random.randn(2)
                force = np.clip(force, -1.0, 1.0)

            vel = self.friction * vel + force
            delta = self.dt * vel
            pos = pos + delta

            dist = np.linalg.norm(delta)
            path_length += dist
            if dist > 1e-8:
                headings.append(np.arctan2(delta[1], delta[0]))

        # Heading angle variance
        if len(headings) >= 2:
            diffs = np.diff(headings)
            diffs = (diffs + np.pi) % (2 * np.pi) - np.pi
            heading_angle_var = float(np.var(diffs))
        else:
            heading_angle_var = 0.0

        return tuple(pos), heading_angle_var, path_length

    def collect(self, agent):
        n_ep = 1 if self.noise_sigma == 0.0 else self.n_episodes

        final_xys = []
        heading_vars = []
        path_lengths = []

        for _ in range(n_ep):
            final_xy, hav, pl = self._simulate(agent)
            final_xys.append(final_xy)
            heading_vars.append(hav)
            path_lengths.append(pl)

        mean_xy = tuple(
            float(np.mean([xy[d] for xy in final_xys]))
            for d in range(2)
        )

        return {
            'end_effector': mean_xy,
            'heading_angle_var': float(np.mean(heading_vars)),
            'path_length': float(np.mean(path_lengths)),
            'max_path_length': self.max_path_length,
        }
