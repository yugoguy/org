from abc import ABC, abstractmethod


class BehaviorDescriptor(ABC):
    """
    Abstract class for behavior descriptor.
    Extracts behavior from raw info dict produced by ProblemSetting.collect().
    """

    @abstractmethod
    def describe(self, info):
        """
        Compute behavior descriptor from collected info.

        Args:
            info: dict of lists from ProblemSetting.collect()

        Returns:
            behavior descriptor (e.g. tuple or array)
        """
        pass
