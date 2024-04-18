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

    @staticmethod
    def create_user_settings_table(database_manager: DatabaseManager) -> None:
        table_name = "UserSettings"
        sql = f"""
        CREATE TABLE IF NOT EXISTS {table_name} (
        user_id INTEGER PRIMARY KEY,
        testing INTEGER,
        interval_in_minutes TEXT,
        execution_frequency_minutes INTEGER,
        percentage_threshold INTEGER,
        under_purchase_percentage REAL,
        under_highest_percentage REAL,
        above_purchase_percentage REAL,
        operation_value_percentage REAL,
        maximum_operation_value REAL,
        FOREIGN KEY (user_id) REFERENCES User(id)
    )
        """
        database_manager.execute_sql(sql, f"Error creating {table_name} table")

