class UserSettings:
    """TODO: Document Class"""

    def __init__(
        self,
        testing: bool = False,
        operation_value_percentage: float = 5.0,
        maximum_operation_value: float = 100.0,
    ):
        self.testing = testing
        self.operation_value_percentage = operation_value_percentage
        self.maximum_operation_value = maximum_operation_value

    def __str__(self) -> str:
        return f"Testing: {self.testing}\nPercentage Threshold: {self.percentage_threshold}\nUnder Purchase Percentage: {self.under_purchase_percentage}\nUnder Highest Percentage: {self.under_highest_percentage}\nAbove Purchase Percentage: {self.above_purchase_percentage}\nOperation Value Percentage: {self.operation_value_percentage}\nMaximum Operation Value: {self.maximum_operation_value}"
