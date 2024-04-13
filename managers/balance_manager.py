from utils.user_settings import UserSettings
from utils.datetime_utils import DateTimeUtils
from managers.data_manager import DataManager


class BalanceManager:
    def __init__(self, user_settings: UserSettings, data_manager: DataManager):
        self.user_settings = user_settings
        self.data_manager = data_manager

    def has_balance(self, operation_value: float) -> tuple[bool, float]:
        """
        Check if there is enough balance to perform an operation.
        Args:
            operation_value (float): The value of the operation.
        Returns:
            tuple[bool, float]: True if there is enough balance, False otherwise. Balance.
        """
        balance = self.data_manager.get_database_usdt_balance()
        has_balance = balance >= operation_value
        return has_balance, balance

    def update_balance(self, value: float) -> float:
        """
        Update the balance in the database.
        Args:
            value (float): The value to be added or subtracted from the balance.
        Returns:
            float: The new balance after the update.
        """
        balance = self.data_manager.get_database_usdt_balance()
        new_balance = round((balance + value), 2)
        self.data_manager.update_usdt_balance(new_balance)
        return new_balance

    def insert_usdt(self, value: float) -> None:
        """
        Inserts USDT value into the database.
        Args:
            value (float): The USDT value to be inserted.
        """
        current_datetime = DateTimeUtils.get_datetime()
        self.data_manager.insert_usdt(value, current_datetime)

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
