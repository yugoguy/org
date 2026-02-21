from abc import ABC, abstractmethod


class ProblemSetting(ABC):
    """
    Abstract class for problem/environment definition.
    Collects raw episode data from an agent.
    """

    @abstractmethod
    def collect(self, agent):
        """
        Run agent in environment and collect raw info.

        Args:
            agent: agent with act(obs) method, weights already set

        Returns:
            dict of lists, each list has one entry per episode.
            e.g. {'reward': [r1, r2], 'final_x': [x1, x2], ...}
        """
        pass
