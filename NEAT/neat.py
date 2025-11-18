import random, math, numpy as np
from collections import defaultdict
from typing import List, Callable

class NEAT:
    """
    Generic NEAT engine.
      • genomes:   list[Genome]
      • evaluate_fn(genomes) -> np.ndarray[floats]  (higher is better)
    Public:
      evolve(n_generations)
    """
    def __init__(self,
                 pop_size: int,
                 genome_template: 'Genome',
                 evaluate_fn: Callable[[List['Genome']], np.ndarray],
                 elite_percent: float = 0.1,
                 compat_threshold: float = 3.0,
                 mutate_weight_prob: float = 0.8,
                 mutate_add_conn_prob: float = 0.05,
                 mutate_add_node_prob: float = 0.03,
                 weight_sigma: float = 0.2,
                 perturb_prob: float = 0.9,
                 max_conn_tries: int = 20,
                 compat_coefs: tuple = (1.0, 1.0, 0.4)):
        """Create a NEAT engine with adjustable hyper-parameters."""

        self.tbl           = genome_template.tbl               # shared
        self.pop_size      = pop_size
        self.evaluate_fn   = evaluate_fn
        self.elite_percent = elite_percent
        self.compat_thresh = compat_threshold
        self.mutate_weight_prob   = mutate_weight_prob
        self.mutate_add_conn_prob = mutate_add_conn_prob
        self.mutate_add_node_prob = mutate_add_node_prob
        self.weight_sigma        = weight_sigma
        self.perturb_prob        = perturb_prob
        self.max_conn_tries      = max_conn_tries
        self.compat_coefs        = compat_coefs
        # Clone template to make initial homogeneous population
        self.population = [genome_template.clone() for _ in range(pop_size)]
        self.species    = defaultdict(list)   # species_id -> indices
        self.best_genome = None

    # -----------------------------------------------------------------
    # ---   public main loop
    # -----------------------------------------------------------------
    def evolve(self, n_generations: int):
        for gen in range(n_generations):
            # 1. evaluate ------------------------------------------------
            result = self.evaluate_fn(self.population)
            if isinstance(result, tuple):
                fitness, scored, conceded = result
            else:
                fitness = result
                scored = conceded = None
            best_i = int(np.argmax(fitness))
            self.best_genome = self.population[best_i].clone()
            log = (
                f"Gen {gen:02d}  best={fitness[best_i]:.1f}  "
                f"species={len(self.species)}"
            )
            if scored is not None:
                log += (
                    f"  scored={int(scored[best_i])}"
                    f"  conceded={int(conceded[best_i])}"
                    f"  high_scorer={int(np.argmax(scored))}"
                )
            print(log)

            # 2. speciate -----------------------------------------------
            self._update_species()

            # 3. produce next generation -------------------------------
            self.population = self._reproduce(fitness)

    # -----------------------------------------------------------------
    # --- speciation helpers
    # -----------------------------------------------------------------
    def compat_distance(self, g1: 'Genome', g2: 'Genome'):
        """Very rough compat for minimal demo (counts disjoint / excess)."""
        c1, c2, c3 = self.compat_coefs
        g1_ids = {c.innov for c in g1.conns}
        g2_ids = {c.innov for c in g2.conns}
        ex_dis = len(g1_ids.symmetric_difference(g2_ids))
        w_diff = np.mean([abs(cc1.w - cc2.w) for cc1, cc2
                          in zip(g1.conns, g2.conns) if cc1.innov==cc2.innov]) \
                 if g1.conns and g2.conns else 0
        N = max(len(g1.conns), len(g2.conns), 1)
        return (c1*ex_dis)/N + c3*w_diff

    def _update_species(self):
        self.species.clear()
        if not self.population: return
        # first genome defines first species
        reprs = [self.population[0]]
        self.species[0].append(0)
        for i,g in enumerate(self.population[1:], 1):
            for sid, rep in enumerate(reprs):
                if self.compat_distance(g, rep) < self.compat_thresh:
                    self.species[sid].append(i); break
            else:
                reprs.append(g); self.species[len(reprs)-1].append(i)

    # -----------------------------------------------------------------
    # --- reproduction
    # -----------------------------------------------------------------
    def _reproduce(self, fitness: np.ndarray) -> List['Genome']:
        new_pop = []
        # keep top global elites
        n_elite = max(1, int(self.elite_percent * self.pop_size))
        elite_idx = fitness.argsort()[-n_elite:]
        new_pop.extend([self.population[i].clone() for i in elite_idx])

        # fill rest species-wise
        species_fits = {sid: fitness[idx].mean() for sid,idx in self.species.items()}
        total_fit = sum(species_fits.values())
        for sid, members in self.species.items():
            quota = max(1, int(self.pop_size * species_fits[sid]/(total_fit+1e-8)))
            for _ in range(quota):
                p1, p2 = random.sample(members, 2)
                child  = self.mate(self.population[p1], self.population[p2])
                self.mutate(child)
                new_pop.append(child)
        # top up if we’re short
        while len(new_pop) < self.pop_size:
            new_pop.append(random.choice(self.population).clone())
        return new_pop[:self.pop_size]

    # -----------------------------------------------------------------
    # --- genetic ops (minimal versions) ------------------------------
    # -----------------------------------------------------------------
    def mate(self, g1: 'Genome', g2: 'Genome') -> 'Genome':
        """Keep genes with equal innovation from fitter parent (no crossover of topology)."""
        child = g1.clone()
        for c in child.conns:
            # if conn also exists in g2, randomly pick one weight
            match = next((cc for cc in g2.conns if cc.innov==c.innov), None)
            if match and random.random()<0.5:
                c.w = match.w
        return child

    def mutate(self, g: 'Genome'):
        if random.random() < self.mutate_weight_prob:
            g.mutate_weights(sigma=self.weight_sigma,
                            perturb_prob=self.perturb_prob)
        if random.random() < self.mutate_add_conn_prob:
            g.mutate_add_conn(max_tries=self.max_conn_tries)
        if random.random() < self.mutate_add_node_prob:
            g.mutate_add_node()
