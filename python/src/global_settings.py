from typing import List


class GlobalSettings:
    """Represents global settings for the application."""

    INTERVALS_IN_MINUTES: List[int] = [5, 10, 15, 30, 60]
    # List[int]: The intervals, in minutes, used for data collection and analysis.

    EXECUTION_FREQUENCY_MINUTES: float = 2  # 5
    # float: The frequency, in minutes, at which the application executes its operations.

    '''
    BUYING_PERCENTAGE_THRESHOLD: float = 10.0  # 10.0
    # float: The threshold percentage used for buying operations.

    SELLING_UNDER_PURCHASE_PERCENTAGE: float = 3.0  # 3.0
    # float: The percentage below which an asset's price is considered for selling if it is below the purchase price.

    SELLING_UNDER_HIGHEST_PERCENTAGE: float = 3.0  # 3.0
    # float: The percentage below which an asset's price is considered for selling if it is below the highest price reached.

    SELLING_ABOVE_PURCHASE_PERCENTAGE: float = 200.0  # 200.0
    # float: The percentage above which an asset's price is considered for selling if it is above the purchase price.
    '''
