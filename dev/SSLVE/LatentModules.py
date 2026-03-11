import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F


class BaseBetaVAE(nn.Module):
    """
    Beta-VAE with injectable auxiliary losses.

    Args:
        input_dim: input dimension
        latent_dim: latent space dimension
        hidden_dims: list of hidden layer sizes
        beta: KL weight
        aux_losses: list of (weight, AuxLoss) pairs, or None
    """

    def __init__(self, input_dim, latent_dim, hidden_dims=None, beta=1.0, aux_losses=None):
        super().__init__()
        if hidden_dims is None:
            hidden_dims = [32]
        self.input_dim = input_dim
        self.latent_dim = latent_dim
        self.beta = beta
        self.aux_losses = aux_losses or []
        self.skip_training = False

        # Encoder
        enc_layers = []
        in_dim = input_dim
        for h_dim in hidden_dims:
            enc_layers.append(nn.Linear(in_dim, h_dim))
            enc_layers.append(nn.ReLU())
            in_dim = h_dim
        self.encoder_net = nn.Sequential(*enc_layers)
        self.fc_mu = nn.Linear(in_dim, latent_dim)
        self.fc_logvar = nn.Linear(in_dim, latent_dim)

        # Decoder
        dec_layers = []
        in_dim = latent_dim
        for h_dim in reversed(hidden_dims):
            dec_layers.append(nn.Linear(in_dim, h_dim))
            dec_layers.append(nn.ReLU())
            in_dim = h_dim
        dec_layers.append(nn.Linear(in_dim, input_dim))
        self.decoder_net = nn.Sequential(*dec_layers)

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

    def loss(self, x, x_recon, mu, logvar):
        recon_loss = F.mse_loss(x_recon, x, reduction='mean')
        kl_loss = -0.5 * torch.mean(1 + logvar - mu.pow(2) - logvar.exp())
        total = recon_loss + self.beta * kl_loss
        return {'total': total, 'recon': recon_loss, 'kl': kl_loss}

    def fit(self, dataset, bin_ids=None, bins=None, epochs=100, batch_size=32,
            lr=1e-3, device='cpu', verbose=True, val_split=0.2):
        self.to(device)
        if self.skip_training:
            return {}
        has_aux = len(self.aux_losses) > 0 and bin_ids is not None and bins is not None

        # Move aux losses to device and collect their parameters
        params = list(self.parameters())
        for _, aux in self.aux_losses:
            if isinstance(aux, nn.Module):
                aux.to(device)
                params.extend(aux.parameters())

        data = torch.tensor(np.array(dataset), dtype=torch.float32)
        n = len(data)
        if n == 0:
            return {}
        batch_size = min(batch_size, n)

        n_val = int(n * val_split)
        if n - n_val < batch_size:
            n_val = 0
        n_train = n - n_val

        indices = torch.randperm(n).tolist()
        train_indices = indices[:n_train]
        val_indices = indices[n_train:]

        train_loader = torch.utils.data.DataLoader(
            train_indices, batch_size=batch_size, shuffle=True, drop_last=True)
        val_loader = torch.utils.data.DataLoader(
            val_indices, batch_size=batch_size, shuffle=False, drop_last=True)

        optimizer = torch.optim.Adam(params, lr=lr)

        history = {
            'train_total': [], 'train_recon': [], 'train_kl': [],
            'val_total': [], 'val_recon': [], 'val_kl': []
        }
        if has_aux:
            for _, aux in self.aux_losses:
                history[f'train_{aux.name}'] = []

        for epoch in range(epochs):
            self.train()
            epoch_losses = {'total': 0.0, 'recon': 0.0, 'kl': 0.0}
            if has_aux:
                for _, aux in self.aux_losses:
                    epoch_losses[aux.name] = 0.0

            for idx_batch in train_loader:
                idx_batch = idx_batch.tolist()
                batch = data[idx_batch].to(device)

                optimizer.zero_grad()
                x_recon, mu, logvar, z = self.forward(batch)
                loss_dict = self.loss(batch, x_recon, mu, logvar)
                total = loss_dict['total']

                if has_aux:
                    context = {
                        'x': batch,
                        'x_recon': x_recon,
                        'mu': mu,
                        'logvar': logvar,
                        'z': z,
                        'batch_indices': idx_batch,
                        'bin_ids_batch': [bin_ids[i] for i in idx_batch],
                        'bins': bins,
                        'dataset': data,
                        'model': self,
                    }
                    for weight, aux in self.aux_losses:
                        aux_val = aux.compute(**context)
                        total = total + weight * aux_val
                        epoch_losses[aux.name] += aux_val.item()

                total.backward()
                optimizer.step()

                epoch_losses['total'] += loss_dict['total'].item()
                epoch_losses['recon'] += loss_dict['recon'].item()
                epoch_losses['kl'] += loss_dict['kl'].item()

            n_batches = len(train_loader)
            for k in epoch_losses:
                history[f'train_{k}'].append(epoch_losses[k] / n_batches)

            # Validation (base loss only)
            self.eval()
            val_losses = {'total': 0.0, 'recon': 0.0, 'kl': 0.0}
            with torch.no_grad():
                for idx_batch in val_loader:
                    idx_batch = idx_batch.tolist()
                    batch = data[idx_batch].to(device)
                    x_recon, mu, logvar, z = self.forward(batch)
                    loss_dict = self.loss(batch, x_recon, mu, logvar)
                    for k in val_losses:
                        val_losses[k] += loss_dict[k].item()

            n_val_batches = len(val_loader)
            if n_val_batches > 0:
                for k in val_losses:
                    history[f'val_{k}'].append(val_losses[k] / n_val_batches)

            if verbose and (epoch + 1) % 10 == 0:
                msg = (f"Epoch {epoch+1}/{epochs} | "
                       f"Train Total: {history['train_total'][-1]:.4f}, "
                       f"Recon: {history['train_recon'][-1]:.4f}, "
                       f"KL: {history['train_kl'][-1]:.4f}")
                if has_aux:
                    for _, aux in self.aux_losses:
                        msg += f", {aux.name}: {history[f'train_{aux.name}'][-1]:.4f}"
                if n_val_batches > 0:
                    msg += f" | Val Total: {history['val_total'][-1]:.4f}"
                print(msg)

        self.eval()
        return history



class RealNVPPrior(nn.Module):
    """
    RealNVP normalizing flow operating in latent space.
    Used as a learned prior for FlowPriorVAE.

    Args:
        latent_dim: dimension of latent space (must be even)
        num_flows: number of coupling layers
        num_hidden: hidden layer size in scale/translate networks
        num_hidden_layers: number of hidden layers in scale/translate networks
    """

    def __init__(self, latent_dim, num_flows=3, num_hidden=256, num_hidden_layers=2):
        super().__init__()
        assert latent_dim % 2 == 0, "latent_dim must be even for RealNVP"
        self.latent_dim = latent_dim
        self.half_dim = latent_dim // 2
        self.num_flows = num_flows

        def make_net(out_activation=None):
            layers = [nn.Linear(self.half_dim, num_hidden), nn.LeakyReLU()]
            for _ in range(num_hidden_layers):
                layers.append(nn.Linear(num_hidden, num_hidden))
                layers.append(nn.LeakyReLU())
            layers.append(nn.Linear(num_hidden, self.half_dim))
            if out_activation is not None:
                layers.append(out_activation)
            return nn.Sequential(*layers)

        self.s_nets = nn.ModuleList([make_net(out_activation=nn.Tanh()) for _ in range(num_flows)])
        self.t_nets = nn.ModuleList([make_net() for _ in range(num_flows)])

    def _coupling(self, x, index, forward=True):
        xa, xb = torch.chunk(x, 2, dim=1)
        s = self.s_nets[index](xa)
        t = self.t_nets[index](xa)
        if forward:
            yb = (xb - t) * torch.exp(-s)
        else:
            yb = torch.exp(s) * xb + t
        return torch.cat((xa, yb), dim=1), s

    def _permute(self, x):
        return x.flip(1)

    def f(self, x):
        """Forward: encoder z space -> base Gaussian space."""
        log_det_J = x.new_zeros(x.shape[0])
        z = x
        for i in range(self.num_flows):
            z, s = self._coupling(z, i, forward=True)
            z = self._permute(z)
            log_det_J = log_det_J - s.sum(dim=1)
        return z, log_det_J

    def f_inv(self, z):
        """Inverse: base Gaussian space -> encoder z space."""
        x = z
        for i in reversed(range(self.num_flows)):
            x = self._permute(x)
            x, _ = self._coupling(x, i, forward=False)
        return x

    def log_prob(self, x):
        """Log probability of x under the flow prior."""
        z, log_det_J = self.f(x)
        log_p = -0.5 * (np.log(2.0 * np.pi) + z ** 2)
        return log_p.sum(dim=1) + log_det_J


class BaseFlowVAE(BaseBetaVAE):
    """
    Beta-VAE with RealNVP flow prior.
    Inherits encoder/decoder/fit from BaseBetaVAE, overrides loss with
    flow-based KL.

    Args:
        input_dim: input dimension
        latent_dim: latent space dimension (must be even)
        hidden_dims: list of hidden layer sizes for encoder/decoder
        beta: KL weight
        num_flows: number of RealNVP coupling layers
        flow_hidden: hidden size in flow networks
        flow_hidden_layers: number of hidden layers in flow networks
        aux_losses: list of (weight, AuxLoss) pairs, or None
    """

    def __init__(self, input_dim, latent_dim, hidden_dims=None, beta=1.0,
                 num_flows=3, flow_hidden=256, flow_hidden_layers=2,
                 aux_losses=None):
        super().__init__(input_dim, latent_dim, hidden_dims, beta, aux_losses)
        self.flow = RealNVPPrior(
            latent_dim=latent_dim,
            num_flows=num_flows,
            num_hidden=flow_hidden,
            num_hidden_layers=flow_hidden_layers,
        )

    def loss(self, x, x_recon, mu, logvar, z=None):
        recon_loss = F.mse_loss(x_recon, x, reduction='mean')
        if z is None:
            z = self.reparameterize(mu, logvar)
        log_q = -0.5 * (np.log(2.0 * np.pi) + logvar + (z - mu) ** 2 / logvar.exp()).sum(dim=1).mean()
        log_p = self.flow.log_prob(z).mean()
        kl_loss = log_q - log_p
        total = recon_loss + self.beta * kl_loss
        return {'total': total, 'recon': recon_loss, 'kl': kl_loss}

    def translate(self, z):
        """Map from base Gaussian space through flow inverse then decode."""
        return self.decode(self.flow.f_inv(z))

    def fit(self, dataset, bin_ids=None, bins=None, epochs=100, batch_size=32,
            lr=1e-3, device='cpu', verbose=True, val_split=0.2):
        self.to(device)
        if self.skip_training:
            return {}
        has_aux = len(self.aux_losses) > 0 and bin_ids is not None and bins is not None

        params = list(self.parameters())
        for _, aux in self.aux_losses:
            if isinstance(aux, nn.Module):
                aux.to(device)
                params.extend(aux.parameters())

        data = torch.tensor(np.array(dataset), dtype=torch.float32)
        n = len(data)
        if n == 0:
            return {}
        batch_size = min(batch_size, n)

        n_val = int(n * val_split)
        if n - n_val < batch_size:
            n_val = 0
        n_train = n - n_val

        indices = torch.randperm(n).tolist()
        train_indices = indices[:n_train]
        val_indices = indices[n_train:]

        train_loader = torch.utils.data.DataLoader(
            train_indices, batch_size=batch_size, shuffle=True, drop_last=True)
        val_loader = torch.utils.data.DataLoader(
            val_indices, batch_size=batch_size, shuffle=False, drop_last=True)

        optimizer = torch.optim.Adam(params, lr=lr)

        history = {
            'train_total': [], 'train_recon': [], 'train_kl': [],
            'val_total': [], 'val_recon': [], 'val_kl': []
        }
        if has_aux:
            for _, aux in self.aux_losses:
                history[f'train_{aux.name}'] = []

        for epoch in range(epochs):
            self.train()
            epoch_losses = {'total': 0.0, 'recon': 0.0, 'kl': 0.0}
            if has_aux:
                for _, aux in self.aux_losses:
                    epoch_losses[aux.name] = 0.0

            for idx_batch in train_loader:
                idx_batch = idx_batch.tolist()
                batch = data[idx_batch].to(device)

                optimizer.zero_grad()
                x_recon, mu, logvar, z = self.forward(batch)
                loss_dict = self.loss(batch, x_recon, mu, logvar, z=z)
                total = loss_dict['total']

                if has_aux:
                    context = {
                        'x': batch,
                        'x_recon': x_recon,
                        'mu': mu,
                        'logvar': logvar,
                        'z': z,
                        'batch_indices': idx_batch,
                        'bin_ids_batch': [bin_ids[i] for i in idx_batch],
                        'bins': bins,
                        'dataset': data,
                        'model': self,
                    }
                    for weight, aux in self.aux_losses:
                        aux_val = aux.compute(**context)
                        total = total + weight * aux_val
                        epoch_losses[aux.name] += aux_val.item()

                total.backward()
                optimizer.step()

                epoch_losses['total'] += loss_dict['total'].item()
                epoch_losses['recon'] += loss_dict['recon'].item()
                epoch_losses['kl'] += loss_dict['kl'].item()

            n_batches = len(train_loader)
            for k in epoch_losses:
                history[f'train_{k}'].append(epoch_losses[k] / n_batches)

            self.eval()
            val_losses = {'total': 0.0, 'recon': 0.0, 'kl': 0.0}
            with torch.no_grad():
                for idx_batch in val_loader:
                    idx_batch = idx_batch.tolist()
                    batch = data[idx_batch].to(device)
                    x_recon, mu, logvar, z = self.forward(batch)
                    loss_dict = self.loss(batch, x_recon, mu, logvar, z=z)
                    for k in val_losses:
                        val_losses[k] += loss_dict[k].item()

            n_val_batches = len(val_loader)
            if n_val_batches > 0:
                for k in val_losses:
                    history[f'val_{k}'].append(val_losses[k] / n_val_batches)

            if verbose and (epoch + 1) % 10 == 0:
                msg = (f"Epoch {epoch+1}/{epochs} | "
                       f"Train Total: {history['train_total'][-1]:.4f}, "
                       f"Recon: {history['train_recon'][-1]:.4f}, "
                       f"KL: {history['train_kl'][-1]:.4f}")
                if has_aux:
                    for _, aux in self.aux_losses:
                        msg += f", {aux.name}: {history[f'train_{aux.name}'][-1]:.4f}"
                if n_val_batches > 0:
                    msg += f" | Val Total: {history['val_total'][-1]:.4f}"
                print(msg)

        self.eval()
        return history
