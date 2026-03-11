import os
import numpy as np
import torch
import matplotlib.pyplot as plt
from matplotlib.collections import PolyCollection
from scipy.spatial import Voronoi


def save_checkpoint(path, bm, history, sp=None, lm=None):
    """
    Save checkpoint.

    Args:
        path: directory path (created if not exists)
        bm: MAPElitesBM instance
        history: dict from SSLVE.history or MAPElite.history
        sp: BoltzmannMix instance or None (saves allocation/ema history)
        lm: LatentModule instance or None (saves state_dict)
    """
    os.makedirs(path, exist_ok=True)
    np.savez(path + 'bm_data.npz',
             dataset=np.array(bm.dataset),
             fitnesses=np.array(bm.fitnesses),
             bin_ids=np.array(bm.bin_ids, dtype=object))
    np.save(path + 'history.npy', history)
    if sp is not None:
        np.save(path + 'allocation_history.npy', sp.allocation_history)
        np.save(path + 'ema_history.npy', sp.ema_history)
    if lm is not None:
        torch.save(lm.state_dict(), path + 'lm.pt')


def load_checkpoint(path, bm, history_target, sp=None, lm=None, device='cpu'):
    """
    Load checkpoint into existing objects.

    Args:
        path: directory path
        bm: MAPElitesBM instance (bins/dataset/etc rebuilt in place)
        history_target: dict to update (e.g. sslve.history or me.history)
        sp: BoltzmannMix instance or None (loads allocation/ema history)
        lm: LatentModule instance or None (loads state_dict)
        device: device for lm
    """
    data = np.load(path + 'bm_data.npz', allow_pickle=True)
    dataset = list(data['dataset'])
    fitnesses = list(data['fitnesses'])
    bin_ids = list(data['bin_ids'])

    # Rebuild bm.bins from dataset/fitnesses/bin_ids
    bm.bins = {}
    for theta, fitness, bid in zip(dataset, fitnesses, bin_ids):
        bid = tuple(bid) if isinstance(bid, np.ndarray) else bid
        if bid not in bm.bins:
            bm.bins[bid] = []
        bm.bins[bid].append((theta, float(fitness)))
    bm._rebuild()

    loaded_history = np.load(path + 'history.npy', allow_pickle=True).item()
    for k in history_target:
        if k in loaded_history:
            history_target[k] = loaded_history[k]

    if sp is not None:
        alloc_path = path + 'allocation_history.npy'
        ema_path = path + 'ema_history.npy'
        if os.path.exists(alloc_path):
            sp.allocation_history = list(np.load(alloc_path, allow_pickle=True))
        if os.path.exists(ema_path):
            sp.ema_history = list(np.load(ema_path, allow_pickle=True))

    if lm is not None:
        lm_path = path + 'lm.pt'
        if os.path.exists(lm_path):
            lm.load_state_dict(torch.load(lm_path, map_location=device))



def plot_grid_archive(bm, bd, vmin=-500, vmax=0, save_path=None, figsize=(8, 8), cmap='inferno_r'):
    """
    Visualize a grid-based MAP-Elites archive as a 2D heatmap,
    colored by best fitness per bin.

    Args:
        bm: MAPElitesBM instance (loaded archive)
        bd: AntOmniBD_Grid instance (for bin_ranges and bin_sizes)
        save_path: optional path to save figure
        figsize: figure size
        cmap: colormap (default: inferno_r, so lower fitness = brighter)
    """
    nx, ny = bd.bin_sizes
    (x_lo, x_hi), (y_lo, y_hi) = bd.bin_ranges

    # Build fitness grid (NaN = empty)
    grid = np.full((ny, nx), np.nan)
    for bin_id, entries in bm.bins.items():
        ix, iy = bin_id
        best_f = min(f for _, f in entries)
        grid[iy, ix] = best_f

    # Plot
    fig, ax = plt.subplots(1, 1, figsize=figsize)
    ax.set_facecolor('black')
    fig.patch.set_facecolor('black')

    cmap_obj = plt.get_cmap(cmap).copy()
    cmap_obj.set_bad(color='black')

    im = ax.imshow(grid, origin='lower', extent=[x_lo, x_hi, y_lo, y_hi],
                   aspect='equal', cmap=cmap_obj, interpolation='nearest',
                   vmin=vmin, vmax=vmax)

    ax.set_xlabel('x', color='white')
    ax.set_ylabel('y', color='white')
    ax.tick_params(colors='white')

    cbar = plt.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    cbar.set_label('Best Fitness (lower = better)', color='white')
    cbar.ax.yaxis.set_tick_params(color='white')
    plt.setp(cbar.ax.yaxis.get_ticklabels(), color='white')

    n_occupied = len(bm.bins)
    total = nx * ny
    ax.set_title(f'Grid Archive — {n_occupied}/{total} bins occupied '
                 f'({n_occupied/total:.1%} coverage)',
                 color='white', fontsize=13)

    plt.tight_layout()
    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight', facecolor=fig.get_facecolor())
    plt.show()
    plt.close()



def plot_cvt_archive(bm, bd, vmin=0, vmax=5, save_path=None, figsize=(8, 8), cmap='inferno_r'):
    """
    Visualize a CVT-based MAP-Elites archive as a Voronoi tessellation
    clipped to the reachable disk, colored by best fitness per bin.

    Args:
        bm: MAPElitesBM instance (loaded archive)
        bd: PlanarArmBD_CVT instance (for centers and radius)
        save_path: optional path to save figure
        figsize: figure size
        cmap: colormap (default: inferno_r, so lower fitness = brighter)
    """
    centers = bd.centers
    radius = bd.radius
    n_bins = bd.n_bins

    # Best (min) fitness per bin
    best_fitness = {}
    for bin_id, entries in bm.bins.items():
        best_fitness[bin_id] = min(f for _, f in entries)

    # Collect fitness values for colormap normalization
    fitness_vals = list(best_fitness.values())
    if vmin is None:
        vmin = min(fitness_vals) if fitness_vals else 0
    if vmax is None:
        vmax = max(fitness_vals) if fitness_vals else 1

    # Add bounding points far away to make Voronoi regions finite
    far = radius * 10
    bounding = np.array([
        [far, 0], [-far, 0], [0, far], [0, -far],
        [far, far], [-far, far], [far, -far], [-far, -far],
    ])
    all_points = np.vstack([centers, bounding])
    vor = Voronoi(all_points)

    # Clip each Voronoi region to the disk
    def clip_polygon_to_disk(vertices, r, n_arc=64):
        """Clip a convex polygon to a disk of radius r centered at origin."""
        if len(vertices) < 3:
            return np.array([])

        # Sutherland-Hodgman style clipping against circle
        # Approximate circle as high-res polygon
        angles = np.linspace(0, 2 * np.pi, n_arc, endpoint=False)
        circle = np.column_stack([r * np.cos(angles), r * np.sin(angles)])

        from matplotlib.path import Path
        disk_path = Path(circle, closed=True)

        # Simple approach: intersect polygon with disk
        # Use matplotlib clip: compute polygon vertices inside disk + intersection points
        poly = vertices.tolist()
        clipped = []

        n = len(poly)
        for i in range(n):
            curr = np.array(poly[i])
            nxt = np.array(poly[(i + 1) % n])
            curr_in = np.linalg.norm(curr) <= r
            nxt_in = np.linalg.norm(nxt) <= r

            if curr_in and nxt_in:
                clipped.append(nxt)
            elif curr_in and not nxt_in:
                clipped.append(_circle_intersect(curr, nxt, r))
            elif not curr_in and nxt_in:
                clipped.append(_circle_intersect(curr, nxt, r))
                clipped.append(nxt)
            # both outside: check if edge passes through disk
            else:
                pts = _circle_intersect_both(curr, nxt, r)
                if pts is not None:
                    clipped.append(pts[0])
                    clipped.append(pts[1])

        if len(clipped) < 3:
            return np.array([])

        # Add arc segments between consecutive boundary points
        result = []
        for i in range(len(clipped)):
            p = np.array(clipped[i])
            result.append(p)
            q = np.array(clipped[(i + 1) % len(clipped)])
            # If both on boundary, add arc
            if abs(np.linalg.norm(p) - r) < 1e-6 and abs(np.linalg.norm(q) - r) < 1e-6:
                a1 = np.arctan2(p[1], p[0])
                a2 = np.arctan2(q[1], q[0])
                diff = (a2 - a1) % (2 * np.pi)
                if diff > np.pi:
                    diff -= 2 * np.pi
                if abs(diff) > 1e-6:
                    n_pts = max(2, int(abs(diff) / (2 * np.pi) * n_arc))
                    arc_angles = a1 + np.linspace(0, diff, n_pts + 1)[1:-1]
                    for a in arc_angles:
                        result.append(np.array([r * np.cos(a), r * np.sin(a)]))

        return np.array(result) if len(result) >= 3 else np.array([])

    def _circle_intersect(p1, p2, r):
        """Find intersection of line segment p1->p2 with circle of radius r."""
        d = p2 - p1
        a = np.dot(d, d)
        b = 2 * np.dot(p1, d)
        c = np.dot(p1, p1) - r * r
        disc = b * b - 4 * a * c
        if disc < 0:
            return p2
        sq = np.sqrt(disc)
        t1 = (-b - sq) / (2 * a)
        t2 = (-b + sq) / (2 * a)
        for t in [t1, t2]:
            if 0 <= t <= 1:
                return p1 + t * d
        return p1 + max(0, min(1, t1)) * d

    def _circle_intersect_both(p1, p2, r):
        """Find both intersections of segment with circle, or None."""
        d = p2 - p1
        a = np.dot(d, d)
        b = 2 * np.dot(p1, d)
        c = np.dot(p1, p1) - r * r
        disc = b * b - 4 * a * c
        if disc < 0:
            return None
        sq = np.sqrt(disc)
        t1 = (-b - sq) / (2 * a)
        t2 = (-b + sq) / (2 * a)
        pts = []
        for t in sorted([t1, t2]):
            if 0 <= t <= 1:
                pts.append(p1 + t * d)
        return pts if len(pts) == 2 else None

    # Build polygons and colors
    cmap_obj = plt.get_cmap(cmap)
    polygons = []
    colors = []
    empty_polygons = []

    for i in range(n_bins):
        region_idx = vor.point_region[i]
        region = vor.regions[region_idx]
        if -1 in region or len(region) == 0:
            continue
        verts = vor.vertices[region]
        clipped = clip_polygon_to_disk(verts, radius)
        if len(clipped) < 3:
            continue

        if i in best_fitness:
            polygons.append(clipped)
            if vmax > vmin:
                norm_val = (best_fitness[i] - vmin) / (vmax - vmin)
            else:
                norm_val = 0.5
            colors.append(cmap_obj(norm_val))
        else:
            empty_polygons.append(clipped)

    # Plot
    fig, ax = plt.subplots(1, 1, figsize=figsize)
    ax.set_aspect('equal')

    # Empty bins in black
    if empty_polygons:
        empty_col = PolyCollection(empty_polygons, facecolors='black',
                                   edgecolors='#222222', linewidths=0.3)
        ax.add_collection(empty_col)

    # Filled bins
    if polygons:
        filled_col = PolyCollection(polygons, facecolors=colors,
                                    edgecolors='#333333', linewidths=0.3)
        ax.add_collection(filled_col)

    # Disk boundary
    theta = np.linspace(0, 2 * np.pi, 200)
    ax.plot(radius * np.cos(theta), radius * np.sin(theta), 'w-', linewidth=1.5)

    ax.set_xlim(-radius * 1.05, radius * 1.05)
    ax.set_ylim(-radius * 1.05, radius * 1.05)
    ax.set_facecolor('black')
    fig.patch.set_facecolor('black')
    ax.set_xlabel('x', color='white')
    ax.set_ylabel('y', color='white')
    ax.tick_params(colors='white')

    # Colorbar
    sm = plt.cm.ScalarMappable(cmap=cmap_obj,
                               norm=plt.Normalize(vmin=vmin, vmax=vmax))
    sm.set_array([])
    cbar = plt.colorbar(sm, ax=ax, fraction=0.046, pad=0.04)
    cbar.set_label('Best Fitness (lower = better)', color='white')
    cbar.ax.yaxis.set_tick_params(color='white')
    plt.setp(cbar.ax.yaxis.get_ticklabels(), color='white')

    n_occupied = len(best_fitness)
    ax.set_title(f'CVT Archive — {n_occupied}/{n_bins} bins occupied '
                 f'({n_occupied/n_bins:.1%} coverage)',
                 color='white', fontsize=13)

    plt.tight_layout()
    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight', facecolor=fig.get_facecolor())
    plt.show()
    plt.close()
