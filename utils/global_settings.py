from typing import List


class GlobalSettings:
    """TODO: Document Class"""

    INTERVALS_IN_MINUTES: List[int] = [5, 10, 15, 30, 60]
    EXECUTION_FREQUENCY_MINUTES: int = 5  # 5
    BUYING_PERCENTAGE_THRESHOLD: float = 1.5  # 10.0
    SELLING_UNDER_PURCHASE_PERCENTAGE: float = 1.0  # 3.0
    SELLING_UNDER_HIGHEST_PERCENTAGE: float = 3.0  # 3.0
    SELLING_ABOVE_PURCHASE_PERCENTAGE: float = 200.0  # 200.0

