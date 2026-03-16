import torch
import torch.nn as nn
from abc import ABC, abstractmethod
from output_heads import OutputHead


class BaseModel(ABC, nn.Module):
    def __init__(self, output_head: OutputHead):
        super().__init__()
        self.output_head = output_head

    @abstractmethod
    def extract_features(self, x: torch.Tensor) -> torch.Tensor:
        """Extract hidden representation from input."""
        pass

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        h = self.extract_features(x)
        return self.output_head(h)

    def mc_forward(self, x: torch.Tensor, num_mc: int) -> list[torch.Tensor]:
        self.train()  # enable dropout
        outputs = []
        with torch.no_grad():
            for _ in range(num_mc):
                outputs.append(self.forward(x))
        self.eval()
        return outputs

    def fit(self, train_loader, val_loader=None, epochs: int = 100, lr: float = 1e-3):
        optimizer = torch.optim.Adam(self.parameters(), lr=lr)
        device = next(self.parameters()).device

        for epoch in range(epochs):
            self.train()
            train_loss = 0.0
            for x, y in train_loader:
                x, y = x.to(device), y.to(device)
                optimizer.zero_grad()
                output = self.forward(x)
                loss = self.output_head.loss(output, y)
                loss.backward()
                optimizer.step()
                train_loss += loss.item()
            train_loss /= len(train_loader)

            if val_loader is not None:
                self.eval()
                val_loss = 0.0
                with torch.no_grad():
                    for x, y in val_loader:
                        x, y = x.to(device), y.to(device)
                        output = self.forward(x)
                        val_loss += self.output_head.loss(output, y).item()
                val_loss /= len(val_loader)
                print(f"Epoch {epoch+1}/{epochs} - train_loss: {train_loss:.4f} - val_loss: {val_loss:.4f}")
            else:
                print(f"Epoch {epoch+1}/{epochs} - train_loss: {train_loss:.4f}")
