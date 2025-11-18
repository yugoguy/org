"""Utility helpers for running and visualising trained genomes."""

from typing import Optional, Sequence
import numpy as np
import jax
import jax.numpy as jnp
import imageio

from genome import Genome
from slimevolly import SlimeVolleyTask
from evojax.task.slimevolley import SlimeVolley


def logits_to_buttons(logits: Sequence[float]) -> np.ndarray:
    """Convert action logits (length 8) to binary button array (left,right,jump)."""
    idx = int(np.argmax(np.asarray(logits)))
    return np.array([(idx >> 0) & 1, (idx >> 1) & 1, (idx >> 2) & 1], dtype=np.float32)


def rollout_frames(
    genome: Genome,
    steps: int = 400,
    frame_skip: int = 5,
    seed: int = 0,
    env: Optional[SlimeVolley] = None,
):
    """Roll a genome in the environment and capture frames."""

    if env is None:
        env = SlimeVolley(max_steps=1000, test=True)
    fwd = genome.forward_jax()
    state = env.reset(jax.random.split(jax.random.PRNGKey(seed), 1))

    frames = []
    for step in range(steps):
        obs = state.obs[0]
        logits = fwd(jnp.asarray(obs))
        action = logits_to_buttons(logits)[None, :]
        state, _, _ = env.step(state, action)
        if step % frame_skip == 0:
            s0 = jax.tree_util.tree_map(lambda x: x[0], state)
            frames.append(np.array(SlimeVolley.render(s0)))
    return frames


def save_gif(frames, filename: str, fps: int = 20, scale: int = 2) -> None:
    """Save a list of RGB frames to a GIF file."""

    if scale != 1:
        frames = [np.kron(f, np.ones((scale, scale, 1), dtype=f.dtype)) for f in frames]
    imageio.mimsave(filename, frames, duration=int(1000 / fps))


__all__ = ["logits_to_buttons", "rollout_frames", "save_gif"]
