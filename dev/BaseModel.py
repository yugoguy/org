import torch
import torch.nn as nn
from abc import ABC, abstractmethod
from OutputHeads import OutputHead


class BaseModel(ABC, nn.Module):
    def __init__(self, output_head: OutputHead):
        super().__init__()
        self.output_head = output_head

    @abstractmethod
    def extract_features(self, x: torch.Tensor) -> torch.Tensor:
        pass

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        h = self.extract_features(x)
        return self.output_head(h)

    def mc_forward(self, x: torch.Tensor, num_mc: int) -> list[torch.Tensor]:
        self.train()
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


class TemporalBlock(nn.Module):
    def __init__(self, in_channels: int, out_channels: int, kernel_size: int, dilation: int, dropout: float = 0.0):
        super().__init__()
        padding = (kernel_size - 1) * dilation

        self.conv1 = nn.Conv1d(in_channels, out_channels, kernel_size, dilation=dilation, padding=padding)
        self.conv2 = nn.Conv1d(out_channels, out_channels, kernel_size, dilation=dilation, padding=padding)
        self.relu1 = nn.ReLU()
        self.relu2 = nn.ReLU()
        self.dropout1 = nn.Dropout(dropout) if dropout > 0 else nn.Identity()
        self.dropout2 = nn.Dropout(dropout) if dropout > 0 else nn.Identity()
        self.residual = nn.Conv1d(in_channels, out_channels, 1) if in_channels != out_channels else nn.Identity()
        self.padding = padding

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        out = self.conv1(x)[:, :, :x.size(2)]
        out = self.dropout1(self.relu1(out))
        out = self.conv2(out)[:, :, :x.size(2)]
        out = self.dropout2(self.relu2(out))
        return out + self.residual(x)


class TCN(BaseModel):
    def __init__(self, in_channels: int, num_channels_list: list[int], kernel_size: int,
                 output_head: OutputHead, dropout: float = 0.0):
        super().__init__(output_head)
        blocks = []
        for i, out_channels in enumerate(num_channels_list):
            dilation = 2 ** i
            blocks.append(TemporalBlock(in_channels, out_channels, kernel_size, dilation, dropout))
            in_channels = out_channels
        self.network = nn.Sequential(*blocks)

    def extract_features(self, x: torch.Tensor) -> torch.Tensor:
        out = self.network(x)
        return out[:, :, -1]
