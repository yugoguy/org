import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F


class BetaVAE_SSLVE(LatentModule):
    """
    Beta-VAE with non-contrastive SSL loss for SSLVE.

    SSL loss aligns enc(theta) toward stop-gradient enc(theta') for all
    bin-mates theta' in B(theta)\{theta}, normalized by (|B(theta)|-1) * latent_dim.
    Samples with no bin-mates contribute 0 to SSL loss.

    Total loss = VAE loss + gamma_ssl * SSL loss

    Args:
        input_dim: input dimension
        latent_dim: latent space dimension
        hidden_dim: hidden layer size
        beta: KL weight
        gamma_ssl: SSL loss weight
    """

    def __init__(self, input_dim, latent_dim, hidden_dim=32, beta=1.0, gamma_ssl=1.0):
        super().__init__(input_dim, latent_dim)
        self.beta = beta
        self.gamma_ssl = gamma_ssl

        self.encoder_net = nn.Sequential(
            nn.Linear(input_dim, hidden_dim),
            nn.ReLU(),
        )
        self.fc_mu = nn.Linear(hidden_dim, latent_dim)
        self.fc_logvar = nn.Linear(hidden_dim, latent_dim)

        self.decoder_net = nn.Sequential(
            nn.Linear(latent_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, input_dim),
        )

    def encode(self, x):
        h = self.encoder_net(x)
        return self.fc_mu(h)

    def encode_dist(self, x):
        h = self.encoder_net(x)
        mu = self.fc_mu(h)
        logvar = torch.clamp(self.fc_logvar(h), min=-20, max=2)
        return mu, logvar

    def reparameterize(self, mu, logvar):
        std = torch.exp(0.5 * logvar)
        return mu + torch.randn_like(std) * std

    def decode(self, z):
        return self.decoder_net(z)

    def forward(self, x):
        mu, logvar = self.encode_dist(x)
        z = self.reparameterize(mu, logvar)
        x_recon = self.decode(z)
        return x_recon, mu, logvar, z

    def nograd_encode(self, x):
        """Stop-gradient encode for SSL target."""
        with torch.no_grad():
            return self.encode(x)

    def nograd_encode_dist(self, x):
        """Stop-gradient encode returning (mu, logvar) for SSL target."""
        with torch.no_grad():
            return self.encode_dist(x)

    def loss(self, x, x_recon, mu=None, logvar=None, z=None, return_parts=False, **kwargs):
        recon_loss = F.mse_loss(x_recon, x, reduction='mean')
        kl_loss = -0.5 * torch.mean(1 + logvar - mu.pow(2) - logvar.exp())
        total = recon_loss + self.beta * kl_loss
        if return_parts:
            return {'total': total, 'recon': recon_loss, 'kl': kl_loss}
        return total

    def ssl_loss(self, mu_batch, logvar_batch, mu_neighbors, logvar_neighbors, counts):
        """
        KL-based SSL loss: KL(q(z|θ') || q(z|θ)) where θ' is stop-grad neighbor.
        Gradients flow through θ (batch), encouraging it to cover frozen neighbors.

        Args:
            mu_batch: (B, latent_dim) with grad
            logvar_batch: (B, latent_dim) with grad
            mu_neighbors: (total_neighbors, latent_dim) stop-grad
            logvar_neighbors: (total_neighbors, latent_dim) stop-grad
            counts: list of int, number of neighbors per sample

        Returns:
            scalar SSL loss
        """
        total_neighbors = mu_neighbors.size(0)
        if total_neighbors == 0:
            return torch.tensor(0.0, device=mu_batch.device)

        counts_tensor = torch.tensor(counts, dtype=torch.long, device=mu_batch.device)
        # Expand batch to align with stacked neighbors
        mu_b = torch.repeat_interleave(mu_batch, counts_tensor, dim=0)
        logvar_b = torch.repeat_interleave(logvar_batch, counts_tensor, dim=0)

        # KL(N(mu', sigma'^2) || N(mu, sigma^2)) per dimension
        # = log(sigma/sigma') + (sigma'^2 + (mu'-mu)^2) / (2*sigma^2) - 0.5
        var_b = logvar_b.exp()
        var_n = logvar_neighbors.exp()
        kl = 0.5 * (logvar_b - logvar_neighbors + var_n / var_b
                     + (mu_neighbors - mu_b).pow(2) / var_b - 1.0)
        kl_per_neighbor = kl.sum(dim=1)  # (total_neighbors,)

        # Average per sample: sum over neighbors / count, then mean over batch
        loss = torch.tensor(0.0, device=mu_batch.device)
        offset = 0
        for c in counts:
            if c > 0:
                loss = loss + kl_per_neighbor[offset:offset + c].mean()
            offset += c

        return loss / mu_batch.size(0)

    def fit(self, dataset, bin_ids, bins, epochs=100, batch_size=32,
            lr=1e-3, device='cpu', verbose=True, val_split=0.2):
        """
        Training loop with SSL loss.

        Args:
            dataset: list of numpy arrays (raw, no normalization)
            bin_ids: list of bin IDs aligned with dataset
            bins: dict {bin_id: [dataset_indices]}
            epochs, batch_size, lr, device, verbose, val_split: standard training args

        Returns:
            dict with train/val losses per epoch
        """
        self.to(device)

        data = torch.tensor(np.array(dataset), dtype=torch.float32)
        n = len(data)
        n_val = int(n * val_split)
        n_train = n - n_val

        indices = torch.randperm(n).tolist()
        train_indices = indices[:n_train]
        val_indices = indices[n_train:]

        train_loader = torch.utils.data.DataLoader(
            train_indices, batch_size=batch_size, shuffle=True, drop_last=True)
        val_loader = torch.utils.data.DataLoader(
            val_indices, batch_size=batch_size, shuffle=False, drop_last=True)

        optimizer = torch.optim.Adam(self.parameters(), lr=lr)

        history = {
            'train_total': [], 'train_recon': [], 'train_kl': [], 'train_ssl': [],
            'val_total': [], 'val_recon': [], 'val_kl': []
        }

        for epoch in range(epochs):
            self.train()
            epoch_losses = {'total': 0.0, 'recon': 0.0, 'kl': 0.0, 'ssl': 0.0}

            for idx_batch in train_loader:
                idx_batch = idx_batch.tolist()
                batch = data[idx_batch].to(device)

                optimizer.zero_grad()
                x_recon, mu, logvar, z = self.forward(batch)
                loss_dict = self.loss(batch, x_recon, mu=mu, logvar=logvar, z=z, return_parts=True)

                # Build stacked neighbor tensor
                neighbor_list = []
                counts = []
                for i in idx_batch:
                    bid = bin_ids[i]
                    neighbor_indices = [j for j in bins[bid] if j != i]
                    counts.append(len(neighbor_indices))
                    if neighbor_indices:
                        neighbor_list.append(data[neighbor_indices])

                if neighbor_list:
                    stacked = torch.cat(neighbor_list, dim=0).to(device)
                    mu_neighbors, logvar_neighbors = self.nograd_encode_dist(stacked)
                else:
                    mu_neighbors = torch.empty(0, self.latent_dim, device=device)
                    logvar_neighbors = torch.empty(0, self.latent_dim, device=device)

                ssl = self.ssl_loss(mu, logvar, mu_neighbors, logvar_neighbors, counts)
                total = loss_dict['total'] + self.gamma_ssl * ssl

                total.backward()
                optimizer.step()

                epoch_losses['total'] += loss_dict['total'].item()
                epoch_losses['recon'] += loss_dict['recon'].item()
                epoch_losses['kl'] += loss_dict['kl'].item()
                epoch_losses['ssl'] += ssl.item()

            n_batches = len(train_loader)
            for k in epoch_losses:
                history[f'train_{k}'].append(epoch_losses[k] / n_batches)

            # Validation (VAE loss only)
            self.eval()
            val_losses = {'total': 0.0, 'recon': 0.0, 'kl': 0.0}
            with torch.no_grad():
                for idx_batch in val_loader:
                    idx_batch = idx_batch.tolist()
                    batch = data[idx_batch].to(device)
                    x_recon, mu, logvar, z = self.forward(batch)
                    loss_dict = self.loss(batch, x_recon, mu=mu, logvar=logvar, z=z, return_parts=True)
                    for k in val_losses:
                        val_losses[k] += loss_dict[k].item()

            n_val_batches = len(val_loader)
            for k in val_losses:
                history[f'val_{k}'].append(val_losses[k] / n_val_batches)

            if verbose and (epoch + 1) % 10 == 0:
                print(f"Epoch {epoch+1}/{epochs} | "
                      f"Train Total: {history['train_total'][-1]:.4f}, "
                      f"Recon: {history['train_recon'][-1]:.4f}, "
                      f"KL: {history['train_kl'][-1]:.4f}, "
                      f"SSL: {history['train_ssl'][-1]:.4f} | "
                      f"Val Total: {history['val_total'][-1]:.4f}")

        self.eval()
        return history
