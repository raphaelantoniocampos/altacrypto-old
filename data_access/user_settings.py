class UserSettings:
    """TODO: Document Class"""

    def __init__(
        self,
        testing: bool = True,
        interval_in_minutes: list = [5, 10, 15, 30, 60],
        execution_frequency_minutes: int = 5,
        percentage_threshold: int = 10,
        under_purchase_percentage: float = 3.0,
        under_highest_percentage: float = 3.0,
        above_purchase_percentage: float = 200.0,
        operation_value_percentage: float = 5,
        maximum_operation_value: float = 100.0,
    ):
        self.testing = testing
        self.interval_in_minutes = interval_in_minutes
        self.execution_frequency_minutes = execution_frequency_minutes
        self.percentage_threshold = percentage_threshold
        self.under_purchase_percentage = under_purchase_percentage
        self.under_highest_percentage = under_highest_percentage
        self.above_purchase_percentage = above_purchase_percentage
        self.operation_value_percentage = operation_value_percentage
        self.maximum_operation_value = maximum_operation_value
