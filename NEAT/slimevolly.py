import jax, jax.numpy as jnp
from evojax.task.slimevolley import SlimeVolley

class SlimeVolleyTask:
    """
    Thin adapter around evojax.task.slimevolley.SlimeVolley
    that:
      • creates N parallel games on GPU,
      • exposes reset(step_rng_batch) / step(actions) like a gym VecEnv,
      • keeps observations as float32 (0-1) flat tensors.
    """
    def __init__(self, pop_size: int, max_steps: int = 1000, *, seed: int = 0, test: bool = True):
        """Create a batch of ``pop_size`` parallel games.

        ``seed`` controls the random key for resets and ``test`` is forwarded to
        :class:`SlimeVolley`.
        """
        self.pop = pop_size
        self.env = SlimeVolley(max_steps=max_steps, test=test)
        self.key = jax.random.PRNGKey(seed)

    def reset(self):
        self.key, sub = jax.random.split(self.key)
        keys = jax.random.split(sub, self.pop)
        return self.env.reset(keys)              # shape (pop, obs_dim)

    def step(self, state, actions):
        """
        actions : float32 (pop, 8) softmax logits  -> argmax inside
        Returns (next_state, reward, done) each shape (pop,)
        """
        next_state, reward, done = self.env.step(state, actions)
        return next_state, reward, done


class SlimeVolleySelfPlayTask:
    """A variant of :class:`SlimeVolleyTask` that supports self-play.

    Each environment instance hosts a match between two agents. The ``pop``
    argument specifies the number of parallel matches running on the GPU.
    """

    def __init__(self, n_pairs: int, max_steps: int = 1000, *, seed: int = 0, test: bool = True):
        """Create ``n_pairs`` self-play environments.

        ``seed`` initialises the random key and ``test`` is forwarded to
        :class:`SlimeVolley`.
        """
        self.n_pairs = n_pairs
        self.env = SlimeVolley(max_steps=max_steps, test=test, self_play=True)
        self.key = jax.random.PRNGKey(seed)

    def reset(self):
        self.key, sub = jax.random.split(self.key)
        keys = jax.random.split(sub, self.n_pairs * 2)
        keys = keys.reshape(self.n_pairs, 2, -1)
        return self.env.reset(keys)

    def step(self, state, actions_a, actions_b):
        """Step the environment with actions for both players."""
        actions = jnp.stack([actions_a, actions_b], axis=1)
        next_state, reward, done = self.env.step(state, actions)
        return next_state, reward, done
