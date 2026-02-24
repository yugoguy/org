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
