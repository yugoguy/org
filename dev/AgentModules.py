import numpy as np


class MLP_Agent:
    """
    MLP agent that takes external flat weight vector.
    ReLU hidden activations, configurable output activation.

    Args:
        architecture: list of layer dims, e.g. [4, 32, 2]
        output_activation: 'argmax' (discrete) or 'tanh' (continuous)
    """

    def __init__(self, architecture, output_activation='argmax'):
        self.architecture = architecture
        self.output_activation = output_activation
        self.weights = []
        self.biases = []

    def get_weight_dim(self):
        """Total number of parameters in flat weight vector."""
        dim = 0
        for i in range(len(self.architecture) - 1):
            dim += self.architecture[i] * self.architecture[i + 1]  # weight matrix
            dim += self.architecture[i + 1]  # bias
        return dim

    def set_weights(self, flat_weights):
        """
        Unflatten weight vector into layer weights and biases.

        Args:
            flat_weights: 1D numpy array of length get_weight_dim()
        """
        self.weights = []
        self.biases = []
        offset = 0
        for i in range(len(self.architecture) - 1):
            fan_in = self.architecture[i]
            fan_out = self.architecture[i + 1]
            w_size = fan_in * fan_out
            W = flat_weights[offset:offset + w_size].reshape(fan_in, fan_out)
            offset += w_size
            b = flat_weights[offset:offset + fan_out]
            offset += fan_out
            self.weights.append(W)
            self.biases.append(b)

    def act(self, obs):
        """
        Forward pass. ReLU for hidden layers, output_activation for output.

        Args:
            obs: 1D numpy array (observation)

        Returns:
            int action (argmax) or numpy array (tanh)
        """
        x = np.array(obs, dtype=np.float64)
        for i in range(len(self.weights) - 1):
            x = x @ self.weights[i] + self.biases[i]
            x = np.maximum(x, 0)
        x = x @ self.weights[-1] + self.biases[-1]

        if self.output_activation == 'argmax':
            return int(np.argmax(x))
        elif self.output_activation == 'tanh':
            return np.tanh(x)
        else:
            return x
