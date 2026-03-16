import torch
from BaseModels import BaseModel


class DeepEnsemble:
    def __init__(self, model_class, num_members: int, **model_kwargs):
        self.members = [model_class(**model_kwargs) for _ in range(num_members)]

    def to(self, device):
        for m in self.members:
            m.to(device)
        return self

    def forward(self, x: torch.Tensor) -> list[torch.Tensor]:
        outputs = []
        with torch.no_grad():
            for m in self.members:
                m.eval()
                outputs.append(m(x))
        return outputs

    def fit(self, train_loader, val_loader=None, epochs: int = 100, lr: float = 1e-3):
        for i, member in enumerate(self.members):
            print(f"Training member {i+1}/{len(self.members)}")
            member.fit(train_loader, val_loader, epochs, lr)
