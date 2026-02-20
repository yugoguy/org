from abc import ABC, abstractmethod


class BehaviorDescriptor(ABC):
    """
    Abstract class for behavior descriptor.

    Args:
        problem: ProblemSetting instance
    """

    def __init__(self, problem):
        self.problem = problem

    @abstractmethod
    def describe(self, theta):
        """
        Compute behavior descriptor for a single gene theta.

        Args:
            theta: numpy array, a single solution

        Returns:
            behavior descriptor (e.g. scalar, tuple, or array)
        """
        pass
