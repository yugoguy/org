import os
import numpy as np
import torch


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
