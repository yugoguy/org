import torch
import torch.nn as nn
from base_model import BaseModel
from output_heads import OutputHead


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
        out = self.conv1(x)[:, :, :x.size(2)]  # causal trim
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
        # x: (batch, channels, seq_len)
        out = self.network(x)
        return out[:, :, -1]  # last timestep: (batch, hidden)
