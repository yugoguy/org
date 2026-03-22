from abc import ABC, abstractmethod
import numpy as np
import gymnasium as gym



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



class PointMassCollector:
    """
    Collector for ND point mass trajectory generation.

    Dynamics:
        vel = friction * vel + force
        pos = pos + dt * vel

    Agent input: (pos_1..N, vel_1..N, t/T) — (2N+1)D
    Agent output: (force_1..N) — ND, tanh-bounded [-1, 1]

    Final position normalized by max_path_length = n_steps * dt / (1 - friction).

    Args:
        space_dim: dimension of space (default 2)
        friction: velocity decay factor per step (0 < friction < 1)
        dt: time step size
        n_steps: number of simulation steps per episode
        noise_sigma: std of Gaussian noise added to force per episode (0 = deterministic)
        n_episodes: number of noised episodes to average (ignored if noise_sigma=0)
    """

    def __init__(self, space_dim=2, friction=0.9, dt=0.1, n_steps=100,
                 noise_sigma=0.0, n_episodes=1):
        self.space_dim = space_dim
        self.friction = friction
        self.dt = dt
        self.n_steps = n_steps
        self.noise_sigma = noise_sigma
        self.n_episodes = n_episodes
        self.max_path_length = n_steps * dt / (1.0 - friction)

    def _simulate(self, agent):
        """Run one episode, return final_pos, heading_angle_var, path_length."""
        N = self.space_dim
        pos = np.zeros(N)
        vel = np.zeros(N)
        T = self.n_steps

        displacements = []
        path_length = 0.0

        for t in range(1, T + 1):
            obs = np.concatenate([pos, vel, [t / T]])
            force = agent.act(obs)

            if self.noise_sigma > 0.0:
                force = force + self.noise_sigma * np.random.randn(N)
                force = np.clip(force, -1.0, 1.0)

            vel = self.friction * vel + force
            delta = self.dt * vel
            pos = pos + delta

            dist = np.linalg.norm(delta)
            path_length += dist
            if dist > 1e-8:
                displacements.append(delta.copy())

        # Heading angle variance via angle between consecutive displacements
        if len(displacements) >= 2:
            angles = []
            for i in range(len(displacements) - 1):
                d1 = displacements[i]
                d2 = displacements[i + 1]
                cos_a = np.dot(d1, d2) / (np.linalg.norm(d1) * np.linalg.norm(d2))
                cos_a = np.clip(cos_a, -1.0, 1.0)
                angles.append(np.arccos(cos_a))
            heading_angle_var = float(np.var(angles))
        else:
            heading_angle_var = 0.0

        return tuple(pos), heading_angle_var, path_length

    def collect(self, agent):
        n_ep = 1 if self.noise_sigma == 0.0 else self.n_episodes
        N = self.space_dim

        final_positions = []
        heading_vars = []
        path_lengths = []

        for _ in range(n_ep):
            final_pos, hav, pl = self._simulate(agent)
            final_positions.append(final_pos)
            heading_vars.append(hav)
            path_lengths.append(pl)

        mean_pos = tuple(
            float(np.mean([p[d] for p in final_positions]))
            for d in range(N)
        )
        normalized_pos = tuple(v / self.max_path_length for v in mean_pos)

        return {
            'end_effector': normalized_pos,
            'heading_angle_var': float(np.mean(heading_vars)),
            'path_length': float(np.mean(path_lengths)),
            'max_path_length': self.max_path_length,
        }
