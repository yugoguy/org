import numpy as np

class SphereProblem(ProblemSetting):
    """
    Sphere function with C1 constraint.
    fitness: f(x) = sum(x_i^2)
    constraint: sum(45 - x_i) <= 0  (i.e., sum(x_i) >= 45*D)
    """
    
    def __init__(self, dim, constraint_handling='rejection', penalty_coef=1.0):
        super().__init__(constraint_handling, penalty_coef)
        self.dim = dim
    
    def fitness(self, x):
        return sum(xi ** 2 for xi in x)
    
    def constraint(self, x):
        return sum(45 - xi for xi in x)


class NKLandscape(ProblemSetting):
    """
    NK Landscape problem (unconstrained).
    
    Args:
        n: number of loci (binary)
        k: epistatic interactions per locus (0 to n-1)
        adjacent: if True, interactions are adjacent (sliding window); else random
        seed: random seed for generating interaction tables
    """
    
    def __init__(self, n, k, adjacent=True, seed=None):
        super().__init__(constraint_handling='rejection', penalty_coef=0.0)
        self.n = n
        self.k = k
        self.adjacent = adjacent
        
        rng = np.random.RandomState(seed)
        
        # Build interaction neighbors for each locus
        self.neighbors = []
        for i in range(n):
            if adjacent:
                nbrs = [(i + j + 1) % n for j in range(k)]
            else:
                candidates = list(range(n))
                candidates.remove(i)
                nbrs = rng.choice(candidates, size=k, replace=False).tolist()
            self.neighbors.append(nbrs)
        
        # Fitness contribution tables: for each locus, 2^(k+1) entries
        self.tables = []
        for i in range(n):
            table = rng.uniform(0, 1, size=2 ** (k + 1))
            self.tables.append(table)
    
    def _contribution(self, x, i):
        """Fitness contribution of locus i given binary string x."""
        bits = [int(x[i])] + [int(x[j]) for j in self.neighbors[i]]
        index = 0
        for b in bits:
            index = (index << 1) | b
        return self.tables[i][index]
    
    def fitness(self, x):
        """Return negative average contribution (to minimize)."""
        avg = sum(self._contribution(x, i) for i in range(self.n)) / self.n
        return -avg
    
    def constraint(self, x):
        """No constraint. Always feasible."""
        return -1.0
    
    def has_constraint(self):
        return False


class C2NKLandscape(NKLandscape):
    """
    NK Landscape with two-sided ratio constraint on fraction of 1s.
    Constraint: ratio_min <= sum(x)/n <= ratio_max
    
    Args:
        n, k, adjacent, seed: same as NKLandscape
        ratio_min: minimum fraction of 1s
        ratio_max: maximum fraction of 1s
        penalty_coef: penalty coefficient for constraint violation
    """
    
    def __init__(self, n, k, adjacent=True, seed=None, ratio_min=0.1, ratio_max=0.3, penalty_coef=1.0):
        super().__init__(n, k, adjacent, seed)
        self.constraint_handling = 'penalty'
        self.penalty_coef = penalty_coef
        self.ratio_min = ratio_min
        self.ratio_max = ratio_max
    
    def constraint(self, x):
        """
        Two-sided ratio constraint.
        Returns max violation (>0 means infeasible, <=0 means feasible).
        """
        ratio = sum(int(xi) for xi in x) / self.n
        lower_violation = self.ratio_min - ratio
        upper_violation = ratio - self.ratio_max
        return max(lower_violation, upper_violation)
    
    def has_constraint(self):
        return True


import gymnasium as gym

class CartPoleProblem(ProblemSetting):
    """
    CartPole-v1 problem. Fitness = negative mean steps alive (minimize).
    No constraint.

    Args:
        max_steps: max steps per episode (CartPole-v1 default 500)
        n_episodes: number of episodes to average over
        seed: random seed for env
    """

    def __init__(self, max_steps=500, n_episodes=5, seed=None):
        super().__init__(constraint_handling='rejection', penalty_coef=0.0)
        self.max_steps = max_steps
        self.n_episodes = n_episodes
        self.seed = seed

    def fitness(self, agent):
        """
        Run agent in CartPole, return negative mean steps (minimize).
        Also stores state trajectories in self.last_trajectories.

        Args:
            agent: agent with act(obs) method, weights already set

        Returns:
            negative mean steps alive
        """
        total_steps = 0
        self.last_trajectories = []

        for ep in range(self.n_episodes):
            env = gym.make('CartPole-v1')
            obs, _ = env.reset(seed=self.seed + ep if self.seed is not None else None)
            trajectory = [obs.copy()]
            steps = 0

            for _ in range(self.max_steps):
                action = agent.act(obs)
                obs, _, terminated, truncated, _ = env.step(action)
                trajectory.append(obs.copy())
                steps += 1
                if terminated or truncated:
                    break

            total_steps += steps
            self.last_trajectories.append(np.array(trajectory))
            env.close()

        return -total_steps / self.n_episodes

    def constraint(self, agent):
        return -1.0

    def has_constraint(self):
        return False

    def get_behavior(self, agent):
        """
        Run agent and return behavior descriptor: (mean_position, mean_angle).
        Uses last_trajectories if available, otherwise runs fitness first.

        Returns:
            (mean_position, mean_angle)
        """
        if not hasattr(self, 'last_trajectories') or not self.last_trajectories:
            self.fitness(agent)

        all_states = np.concatenate(self.last_trajectories, axis=0)
        mean_pos = all_states[:, 0].mean()
        mean_angle = all_states[:, 2].mean()
        return mean_pos, mean_angle

    def render_agent(self, agent):
        """
        Render one episode of the agent in CartPole.

        Args:
            agent: agent with act(obs) method, weights already set

        Returns:
            list of RGB frames
        """
        env = gym.make('CartPole-v1', render_mode='rgb_array')
        obs, _ = env.reset(seed=self.seed if self.seed is not None else None)
        frames = [env.render()]

        for _ in range(self.max_steps):
            action = agent.act(obs)
            obs, _, terminated, truncated, _ = env.step(action)
            frames.append(env.render())
            if terminated or truncated:
                break

        env.close()
        return frames



class BipedalWalkerProblem(ProblemSetting):
    """
    BipedalWalker-v3 problem. Fitness = negative total reward (minimize).
    No constraint. Tracks leg contact flags for behavior descriptor.

    Args:
        max_steps: max steps per episode (default 1600)
        n_episodes: number of episodes to average over
        seed: random seed for env
    """

    def __init__(self, max_steps=1600, n_episodes=3, seed=None):
        super().__init__(constraint_handling='rejection', penalty_coef=0.0)
        self.max_steps = max_steps
        self.n_episodes = n_episodes
        self.seed = seed

    def fitness(self, agent):
        """
        Run agent in BipedalWalker, return negative mean total reward (minimize).
        Stores leg contact histories and final hull x positions.

        Args:
            agent: agent with act(obs) method, weights already set

        Returns:
            negative mean total reward
        """
        total_reward = 0
        self.last_leg_contacts = []
        self.last_final_x = []

        for ep in range(self.n_episodes):
            env = gym.make('BipedalWalker-v3')
            obs, _ = env.reset(seed=self.seed + ep if self.seed is not None else None)
            ep_reward = 0
            ep_contacts = []

            for _ in range(self.max_steps):
                action = agent.act(obs)
                obs, reward, terminated, truncated, _ = env.step(action)
                ep_reward += reward
                ep_contacts.append((obs[8], obs[13]))
                if terminated or truncated:
                    break

            final_x = env.unwrapped.hull.position.x
            total_reward += ep_reward
            self.last_leg_contacts.append(ep_contacts)
            self.last_final_x.append(final_x)
            env.close()

        return -total_reward / self.n_episodes

    def constraint(self, agent):
        return -1.0

    def has_constraint(self):
        return False

    def get_behavior(self, agent):
        """
        Return behavior descriptor: (final_x, leg1_contact_ratio, leg2_contact_ratio).
        Uses cached data if available, otherwise runs fitness first.

        Returns:
            (mean_final_x, leg1_contact_ratio, leg2_contact_ratio)
        """
        if not hasattr(self, 'last_leg_contacts') or not self.last_leg_contacts:
            self.fitness(agent)

        # Final x: average across episodes
        mean_final_x = np.mean(self.last_final_x)

        # Leg contacts: average across all timesteps of all episodes
        all_contacts = []
        for ep_contacts in self.last_leg_contacts:
            all_contacts.extend(ep_contacts)

        contacts = np.array(all_contacts)
        leg1_ratio = contacts[:, 0].mean()
        leg2_ratio = contacts[:, 1].mean()
        return mean_final_x, leg1_ratio, leg2_ratio

    def render_agent(self, agent):
        """
        Render one episode of the agent.

        Returns:
            list of RGB frames
        """
        env = gym.make('BipedalWalker-v3', render_mode='rgb_array')
        obs, _ = env.reset(seed=self.seed if self.seed is not None else None)
        frames = [env.render()]

        for _ in range(self.max_steps):
            action = agent.act(obs)
            obs, _, terminated, truncated, _ = env.step(action)
            frames.append(env.render())
            if terminated or truncated:
                break

        env.close()
        return frames
