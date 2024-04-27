from typing import List


class UserSettings:
    """TODO: Document Class"""

    def __init__(
        self,
        testing: bool = True,
        percentage_threshold: int = 10,
        under_purchase_percentage: float = 3.0,
        under_highest_percentage: float = 3.0,
        above_purchase_percentage: float = 200.0,
        operation_value_percentage: float = 5,
        maximum_operation_value: float = 100.0,
    ):
        self.testing = testing
        self.percentage_threshold = percentage_threshold
        self.under_purchase_percentage = under_purchase_percentage
        self.under_highest_percentage = under_highest_percentage
        self.above_purchase_percentage = above_purchase_percentage
        self.operation_value_percentage = operation_value_percentage
        self.maximum_operation_value = maximum_operation_value

    def get_operation_value(self) -> float:
        """
        Calculates the operation value based on the current USDT balance.
        Returns:
            float: The operation value.
        """
        balance = self.data_manager.get_database_usdt_balance()
        operation_value = round(
            balance / (100 / self.user_settings.operation_value_percentage), 2
        )
        if operation_value < 10:
            operation_value = 10
        if operation_value > self.user_settings.maximum_operation_value:
            operation_value = self.user_settings.maximum_operation_value
        return operation_value
