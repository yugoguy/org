from abc import ABC, abstractmethod
import torch
import torch.nn as nn
import numpy as np



class AuxLoss(ABC):
    """
    Abstract auxiliary loss for latent modules.
    Receives a context dict from the base model's fit loop.
    """

    def __init__(self):
        self.name = "aux"

    @abstractmethod
    def compute(self, **context):
        """
        Compute auxiliary loss.

        Args:
            **context: dict of available quantities from base model's training loop.

        Returns:
            scalar tensor
        """
        pass


class BinPred(AuxLoss, nn.Module):
    """
    Auxiliary loss: predict bin center value from latent z via linear layer.
    Loss is MSE between predicted and target bin center, backpropagated through encoder.

    Args:
        behavior_descriptor: BD instance with bin_value(bin_id) method
        latent_dim: latent space dimension
        output_dim: dimension of bin center value (e.g. 2 for 2D end-effector)
    """

    def __init__(self, behavior_descriptor, latent_dim, output_dim):
        AuxLoss.__init__(self)
        nn.Module.__init__(self)
        self.name = "bin_pred"
        self.bd = behavior_descriptor
        self.predictor = nn.Linear(latent_dim, output_dim)

    def compute(self, **context):
        z = context['z']
        bin_ids_batch = context['bin_ids_batch']
        device = z.device

        targets = torch.tensor(
            np.array([self.bd.bin_value(bid) for bid in bin_ids_batch]),
            dtype=torch.float32,
            device=device,
        )

        pred = self.predictor(z)
        return nn.functional.mse_loss(pred, targets)



class MixBinPred(AuxLoss, nn.Module):
    """
    Mixup-augmented bin center prediction from mixed latents.

    For each sample in the batch, pairs it with a random permutation,
    mixes latents and bin center behaviors with random alpha, then
    predicts the mixed behavior from the mixed latent.

    For FlowVAE, mixing should happen in base (Gaussian) space:
        to_base_fn:   z -> z_base,  e.g. lambda z: model.flow.f(z)[0]
        from_base_fn: z_base -> z,  e.g. model.flow.f_inv
    For BetaVAE, leave both as None (identity).

    Args:
        behavior_descriptor: BD instance with bin_value(bin_id) method
        latent_dim: latent space dimension
        output_dim: dimension of behavior (e.g. 2 for 2D)
        alpha_lo: lower bound for alpha sampling (default -0.25)
        alpha_hi: upper bound for alpha sampling (default 1.25)
        to_base_fn: callable(z_tensor) -> z_base_tensor, or None
        from_base_fn: callable(z_base_tensor) -> z_tensor, or None
    """

    def __init__(self, behavior_descriptor, latent_dim, output_dim,
                 alpha_lo=-0.25, alpha_hi=1.25,
                 to_base_fn=None, from_base_fn=None):
        AuxLoss.__init__(self)
        nn.Module.__init__(self)
        self.name = "mix_bin_pred"
        self.bd = behavior_descriptor
        self.alpha_lo = alpha_lo
        self.alpha_hi = alpha_hi
        self.to_base_fn = to_base_fn
        self.from_base_fn = from_base_fn
        self.predictor = nn.Linear(latent_dim, output_dim)

    def compute(self, **context):
        z = context['z']
        bin_ids_batch = context['bin_ids_batch']
        device = z.device
        B = z.shape[0]

        beh = torch.tensor(
            np.array([self.bd.bin_value(bid) for bid in bin_ids_batch]),
            dtype=torch.float32, device=device,
        )

        perm = torch.randperm(B, device=device)
        z_perm = z[perm]
        beh_perm = beh[perm]

        alpha = torch.rand(B, 1, device=device) * (self.alpha_hi - self.alpha_lo) + self.alpha_lo

        if self.to_base_fn is not None:
            z = self.to_base_fn(z)
            z_perm = self.to_base_fn(z_perm)

        z_mix = alpha * z + (1 - alpha) * z_perm

        if self.from_base_fn is not None:
            z_mix = self.from_base_fn(z_mix)

        beh_mix = alpha * beh + (1 - alpha) * beh_perm

        pred = self.predictor(z_mix)
        return nn.functional.mse_loss(pred, beh_mix)
