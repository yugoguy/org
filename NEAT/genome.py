import dataclasses, random, math, copy
import numpy as np
try:
    import jax, jax.numpy as jnp
    JAX = True
except ImportError:
    JAX = False

# ---------- gene types --------------------------------------------------
@dataclasses.dataclass
class NodeGene:
    id: int
    type: str           # 'in' | 'hid' | 'out'
    bias: float = 0.0

@dataclasses.dataclass
class ConnGene:
    src: int
    dst: int
    w: float
    enabled: bool
    innov: int

# ---------- global innovation table ------------------------------------
class InnovationTable:
    def __init__(self):
        self._d = {}            # (src, dst) -> innovation#
    def get(self, src, dst):
        key = (src, dst)
        if key not in self._d:
            self._d[key] = len(self._d) + 1
        return self._d[key]

# ---------- genome ------------------------------------------------------
class Genome:
    def __init__(self, n_in: int, n_out: int, tbl: InnovationTable):
        self.tbl = tbl
        self.nodes = [NodeGene(i, 'in')              for i in range(n_in)] + \
                     [NodeGene(n_in+i, 'out')        for i in range(n_out)]
        self.conns = []
        # fully connect input→output
        for i in range(n_in):
            for j in range(n_out):
                self.conns.append(ConnGene(
                    src=i, dst=n_in+j,
                    w=random.uniform(-1,1),
                    enabled=True,
                    innov=self.tbl.get(i, n_in+j)
                ))

    # ------------ cloning ------------------------------------------------
    def clone(self):
        g = Genome(0,0,self.tbl)
        g.nodes = copy.deepcopy(self.nodes)
        g.conns = copy.deepcopy(self.conns)
        return g

    # ------------ mutations ---------------------------------------------
    def mutate_weights(self, sigma=0.2, perturb_prob: float = 0.9):
        """Mutate connection weights.

        ``perturb_prob`` controls the chance of perturbing an existing weight
        versus reinitialising it.
        """
        for c in self.conns:
            if random.random() < perturb_prob:       # perturb
                c.w += random.gauss(0, sigma)
            else:                                    # re-init
                c.w  = random.uniform(-1,1)

    def mutate_add_conn(self, max_tries=20):
        for _ in range(max_tries):
            src = random.choice(self.nodes).id
            dst = random.choice([n.id for n in self.nodes if n.type != 'in'])
            if any(c.src==src and c.dst==dst for c in self.conns):
                continue
            self.conns.append(ConnGene(
                src, dst, random.uniform(-1,1), True, self.tbl.get(src,dst)))
            return

    def mutate_add_node(self):
        enabled = [c for c in self.conns if c.enabled]
        if not enabled: return
        conn = random.choice(enabled); conn.enabled = False
        new_id = max(n.id for n in self.nodes) + 1
        self.nodes.append(NodeGene(new_id, 'hid'))
        self.conns += [
            ConnGene(conn.src, new_id, 1.0, True, self.tbl.get(conn.src,new_id)),
            ConnGene(new_id, conn.dst, conn.w, True, self.tbl.get(new_id,conn.dst))
        ]

    # ------------ forward (NumPy) ---------------------------------------
    def forward_np(self, x: np.ndarray) -> np.ndarray:
        n_total = len(self.nodes)
        act = np.zeros(n_total, dtype=np.float32)
        act[:x.size] = x
        # simple topo by node id (valid as long as new nodes get new ids)
        for n in self.nodes:
            if n.type == 'in': continue
            s = sum(act[c.src]*c.w for c in self.conns if c.dst==n.id and c.enabled)
            act[n.id] = math.tanh(n.bias + s)
        outs = [n.id for n in self.nodes if n.type=='out']
        return act[outs]

    # ------------ forward (JAX, JIT) ------------------------------------
    def forward_jax(self):
        if not JAX: raise RuntimeError("JAX not available")
        enabled = [c for c in self.conns if c.enabled]
        src = jnp.array([c.src for c in enabled], jnp.int32)
        dst = jnp.array([c.dst for c in enabled], jnp.int32)
        w   = jnp.array([c.w   for c in enabled], jnp.float32)
        bias= jnp.array([n.bias for n in self.nodes], jnp.float32)
        outs= jnp.array([n.id for n in self.nodes if n.type=='out'], jnp.int32)
        n_total = len(self.nodes)

        @jax.jit
        def _f(x):
            act = jnp.zeros(n_total, jnp.float32).at[:x.shape[0]].set(x)
            # topo pass: iterate len(nodes) times (sufficient for acyclic graph)
            def body(a, _):
                s = jax.ops.segment_sum(a[src]*w, dst, n_total)
                a = jnp.where(jnp.arange(n_total)<x.shape[0], a,
                              jnp.tanh(bias + s))
                return a, None
            act, _ = jax.lax.scan(body, act, None, len(self.nodes))
            return act[outs]
        return _f
