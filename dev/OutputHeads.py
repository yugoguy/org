import torch
import torch.nn as nn
from abc import ABC, abstractmethod


class OutputHead(ABC, nn.Module):
    def __init__(self):
        super().__init__()

    @abstractmethod
    def forward(self, h: torch.Tensor) -> torch.Tensor:
        pass

    @abstractmethod
    def loss(self, output: torch.Tensor, target: torch.Tensor) -> torch.Tensor:
        pass


class DeterministicHead(OutputHead):
    def __init__(self, in_features: int, out_features: int):
        super().__init__()
        self.linear = nn.Linear(in_features, out_features)

    def forward(self, h: torch.Tensor) -> torch.Tensor:
        return self.linear(h)

    def loss(self, output: torch.Tensor, target: torch.Tensor) -> torch.Tensor:
        return nn.functional.mse_loss(output, target)


class GaussianHead(OutputHead):
    def __init__(self, in_features: int, out_features: int):
        super().__init__()
        self.mu_linear = nn.Linear(in_features, out_features)
        self.logvar_linear = nn.Linear(in_features, out_features)

    def forward(self, h: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
        mu = self.mu_linear(h)
        log_var = self.logvar_linear(h)
        return mu, log_var

    def loss(self, output: tuple[torch.Tensor, torch.Tensor], target: torch.Tensor) -> torch.Tensor:
        mu, log_var = output
        # NLL of diagonal Gaussian: 0.5 * (log_var + (target - mu)^2 / exp(log_var))
        return 0.5 * torch.mean(log_var + (target - mu) ** 2 / torch.exp(log_var))


class SplineFlowHead(OutputHead):
    def __init__(self, in_features: int, out_features: int, num_bins: int = 8):
        super().__init__()
        self.out_features = out_features
        self.num_bins = num_bins
        self.param_linear = nn.Linear(in_features, out_features * (num_bins * 3 + 1))

    def _get_spline_params(self, h):
        raw = self.param_linear(h)
        raw = raw.view(h.size(0), self.out_features, self.num_bins * 3 + 1)
        widths = raw[..., :self.num_bins]
        heights = raw[..., self.num_bins:2 * self.num_bins]
        slopes = raw[..., 2 * self.num_bins:]
        return widths, heights, slopes

    @staticmethod
    def _prepare_params(widths, heights, slopes):
        widths = torch.softmax(widths, dim=-1) * 2
        heights = torch.softmax(heights, dim=-1) * 2
        slopes = nn.functional.softplus(slopes)
        cumwidths = nn.functional.pad(torch.cumsum(widths, dim=-1) - 1, (1, 0), value=-1.0)
        cumheights = nn.functional.pad(torch.cumsum(heights, dim=-1) - 1, (1, 0), value=-1.0)
        return widths, heights, slopes, cumwidths, cumheights

    @staticmethod
    def _gather_bin_params(val, cumwidths, cumheights, widths, heights, slopes, search_cumul):
        bin_idx = (val.unsqueeze(-1) >= search_cumul[..., :-1]).sum(-1) - 1
        bin_idx = bin_idx.clamp(0, widths.size(-1) - 1)
        w = widths.gather(-1, bin_idx.unsqueeze(-1)).squeeze(-1)
        h = heights.gather(-1, bin_idx.unsqueeze(-1)).squeeze(-1)
        s_k = slopes.gather(-1, bin_idx.unsqueeze(-1)).squeeze(-1)
        s_k1 = slopes.gather(-1, (bin_idx + 1).unsqueeze(-1)).squeeze(-1)
        cum_w = cumwidths.gather(-1, bin_idx.unsqueeze(-1)).squeeze(-1)
        cum_h = cumheights.gather(-1, bin_idx.unsqueeze(-1)).squeeze(-1)
        return w, h, s_k, s_k1, cum_w, cum_h

    @staticmethod
    def _compute_log_det(h, s_k, s_k1, xi, w):
        den = s_k + (s_k1 + s_k - 2) * xi * (1 - xi)
        num = h * (s_k * xi ** 2 + xi * (1 - xi))
        dnum = 2 * h * s_k * xi * den - num * (s_k1 + s_k - 2) * (1 - 2 * xi)
        dy_dxi = dnum / (den ** 2)
        return torch.log(dy_dxi.abs() + 1e-8) - torch.log(w)

    def _spline_forward(self, z, widths, heights, slopes):
        widths, heights, slopes, cumwidths, cumheights = self._prepare_params(widths, heights, slopes)
        w, h, s_k, s_k1, cum_w, cum_h = self._gather_bin_params(
            z, cumwidths, cumheights, widths, heights, slopes, cumwidths)
        xi = ((z - cum_w) / w).clamp(1e-6, 1 - 1e-6)
        num = h * (s_k * xi ** 2 + xi * (1 - xi))
        den = s_k + (s_k1 + s_k - 2) * xi * (1 - xi)
        y = cum_h + num / den
        log_det = self._compute_log_det(h, s_k, s_k1, xi, w)
        return y, log_det

    def _spline_inverse(self, y, widths, heights, slopes):
        widths, heights, slopes, cumwidths, cumheights = self._prepare_params(widths, heights, slopes)
        w, h, s_k, s_k1, cum_w, cum_h = self._gather_bin_params(
            y, cumwidths, cumheights, widths, heights, slopes, cumheights)
        eta = ((y - cum_h) / h).clamp(1e-6, 1 - 1e-6)
        a = h * (s_k - eta * (s_k1 + s_k - 2))
        b = h * eta * (s_k1 + s_k - 2) - (s_k * h + (y - cum_h) * (s_k1 + s_k - 2))
        c = -s_k * (y - cum_h)
        disc = (b ** 2 - 4 * a * c).clamp(min=0)
        xi = ((-b + torch.sqrt(disc)) / (2 * a + 1e-8)).clamp(1e-6, 1 - 1e-6)
        z = cum_w + w * xi
        log_det = -self._compute_log_det(h, s_k, s_k1, xi, w)
        return z, log_det

    def forward(self, h: torch.Tensor):
        widths, heights, slopes = self._get_spline_params(h)
        z = torch.randn(h.size(0), self.out_features, device=h.device)
        samples, _ = self._spline_forward(z, widths, heights, slopes)
        return samples, (widths, heights, slopes)

    def loss(self, output, target: torch.Tensor) -> torch.Tensor:
        _, (widths, heights, slopes) = output
        z, log_det = self._spline_inverse(target, widths, heights, slopes)
        log_prob = -0.5 * (z ** 2 + torch.log(torch.tensor(2 * torch.pi))) + log_det
        return -log_prob.mean()


class QuantileHead(OutputHead):
    def __init__(self, in_features: int, out_features: int, quantiles: list[float] = [0.1, 0.5, 0.9]):
        super().__init__()
        self.quantiles = quantiles
        self.linear = nn.Linear(in_features, out_features * len(quantiles))
        self.out_features = out_features

    def forward(self, h: torch.Tensor) -> torch.Tensor:
        return self.linear(h).view(h.size(0), len(self.quantiles), self.out_features)

    def loss(self, output: torch.Tensor, target: torch.Tensor) -> torch.Tensor:
        # output: (batch, num_quantiles, out_features), target: (batch, out_features)
        target = target.unsqueeze(1)
        errors = target - output
        q = torch.tensor(self.quantiles, device=output.device).view(1, -1, 1)
        return torch.mean(torch.max(q * errors, (q - 1) * errors))
