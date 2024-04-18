class GlobalSettings:
    """TODO: Document Class"""

    def __init__(
        self,
        interval_in_minutes: list = [5, 10, 15, 30, 60],
        execution_frequency_minutes: int = 5,
    ):
        self.interval_in_minutes = interval_in_minutes
        self.execution_frequency_minutes = execution_frequency_minutes
        self.crypto_prices_db_path = "data\\crypto_prices.db"
        self.user_data_db_path = "data\\user_data.db"
