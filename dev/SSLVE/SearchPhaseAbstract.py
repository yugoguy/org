from abc import ABC, abstractmethod


class SearchPhase(ABC):
    """
    Abstract class for search phase in SSLVE.
    Knows agent class and architecture. Generates candidate thetas
    and converts them to agents.

    Args:
        agent_class: class with set_weights(flat_weights) and act(obs)
        architecture: list of layer dims, e.g. [24, 64, 64, 4]
        agent_kwargs: dict passed to agent_class constructor (e.g. output_activation)
    """

    def __init__(self, agent_class, architecture, agent_kwargs=None):
        self.agent_class = agent_class
        self.architecture = architecture
        self.agent_kwargs = agent_kwargs or {}

    def make_agent(self, theta):
        """
        Convert flat weight vector to agent.

        Args:
            theta: numpy array of flat weights

        Returns:
            agent with weights set
        """
        agent = self.agent_class(self.architecture, **self.agent_kwargs)
        agent.set_weights(theta)
        return agent

    @abstractmethod
    def sample(self, latent_module=None, behavior_matching=None):
        """
        Generate candidate thetas.

        Args:
            latent_module: LatentModule instance (None if not yet trained)
            behavior_matching: BehaviorMatching instance (None at t=0)

        Returns:
            list of numpy arrays (candidate thetas)
        """
        pass
