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
