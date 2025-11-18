"""Training utilities for NEAT on Slime Volley."""

from typing import Callable, List
import numpy as np
import jax
import jax.numpy as jnp

from genome import Genome, InnovationTable
from neat import NEAT
from slimevolly import SlimeVolleyTask, SlimeVolleySelfPlayTask


def _evaluate_factory(task: SlimeVolleyTask) -> Callable[[List[Genome]], tuple[np.ndarray, np.ndarray, np.ndarray]]:
    """Return an evaluation function for the given task.

    The returned function evaluates a list of genomes and
    returns ``(fitness, scored, conceded)`` arrays.
    """

    pop_size = task.pop

    def evaluate(genomes: List[Genome]):
        fwd_fns = [g.forward_jax() for g in genomes]
        state = task.reset()
        fitness = np.zeros(pop_size, dtype=np.float32)
        scored = np.zeros(pop_size, dtype=np.int32)
        conceded = np.zeros(pop_size, dtype=np.int32)
        done = jnp.zeros(pop_size, dtype=bool)
        while not bool(done.all()):
            obs = jax.vmap(lambda s: s.obs)(state)
            logits = jnp.stack([f(obs[i]) for i, f in enumerate(fwd_fns)])
            acts = (
                (jnp.arange(3) & (jnp.argmax(logits, 1)[:, None] >> jnp.arange(3)))
                > 0
            ).astype(jnp.float32)
            state, r, done = task.step(state, acts)
            r_np = np.array(r)
            fitness += np.where(done, 0, r_np)
            scored += (r_np > 0).astype(np.int32)
            conceded += (r_np < 0).astype(np.int32)
        return fitness, scored, conceded

    return evaluate


def _selfplay_evaluate_factory(
    task: SlimeVolleySelfPlayTask,
    pairing_fn: Callable[[int], List[tuple[int, int]]] | None = None,
) -> Callable[[List[Genome]], np.ndarray]:
    """Return an evaluation function for self-play matches."""

    n_pairs = task.n_pairs
    if pairing_fn is None:
        def pairing_fn(n: int) -> List[tuple[int, int]]:
            return [(2 * i, 2 * i + 1) for i in range(n // 2)]

    def evaluate(genomes: List[Genome]) -> np.ndarray:
        assert len(genomes) == n_pairs * 2, "population must be even"
        pairs = pairing_fn(len(genomes))
        fwd = [g.forward_jax() for g in genomes]
        state = task.reset()
        fitness = np.zeros(len(genomes), dtype=np.float32)
        done = jnp.zeros(n_pairs, dtype=bool)
        while not bool(done.all()):
            obs1 = state.obs[:, 0]
            obs2 = state.obs[:, 1]
            logits_a = jnp.stack([fwd[p[0]](obs1[i]) for i, p in enumerate(pairs)])
            logits_b = jnp.stack([fwd[p[1]](obs2[i]) for i, p in enumerate(pairs)])
            state, r, done = task.step(state, logits_a, logits_b)
            r_np = np.array(r)
            for i, (p0, p1) in enumerate(pairs):
                fitness[p0] += r_np[i, 0]
                fitness[p1] += r_np[i, 1]
        return fitness

    return evaluate


def train_slime(
    pop_size: int = 20,
    n_generations: int = 10,
    max_steps: int = 1000,
    neat_kwargs: dict | None = None,
    *,
    seed: int = 0,
    test: bool = True,
    selfplay: bool = False,
    pairing_fn: Callable[[int], List[tuple[int, int]]] | None = None,
) -> Genome:
    """Train a NEAT population on the SlimeVolley task.

    If ``selfplay`` is ``True`` the population is split into pairs that play
    against each other each generation. ``pairing_fn`` may be used to control
    how genomes are matched. ``seed`` and ``test`` are forwarded to the
    environment constructor. Additional NEAT hyper-parameters can be provided
    via ``neat_kwargs``.

    Returns the best genome after ``n_generations``.
    """

    if selfplay:
        if pop_size % 2 != 0:
            raise ValueError("pop_size must be even when selfplay is True")
        task = SlimeVolleySelfPlayTask(pop_size // 2, max_steps=max_steps,
                                       seed=seed, test=test)
    else:
        task = SlimeVolleyTask(pop_size=pop_size, max_steps=max_steps,
                               seed=seed, test=test)
    obs_dim = task.reset().obs.shape[-1]
    tbl = InnovationTable()
    template = Genome(obs_dim, 8, tbl)

    evaluate_fn = (
        _selfplay_evaluate_factory(task, pairing_fn)
        if selfplay
        else _evaluate_factory(task)
    )
    if neat_kwargs is None:
        neat_kwargs = {}
    neat = NEAT(pop_size=pop_size,
                genome_template=template,
                evaluate_fn=evaluate_fn,
                **neat_kwargs)
    neat.evolve(n_generations)
    return neat.best_genome


__all__ = ["train_slime"]
