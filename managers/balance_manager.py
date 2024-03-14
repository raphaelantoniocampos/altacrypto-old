import utils.settings as settings
from utils.datetime_utils import DateTimeUtils

class BalanceManager:
    def __init__(self, data_manager):
        self.data_manager = data_manager
    def has_balance(self, operation_value):
        """
        Check if there is enough balance to perform an operation.
        Args:
            operation_value (float): The value of the operation.
        Returns:
            bool: True if there is enough balance, False otherwise.
        """
        balance = self.data_manager.get_database_usdt_balance()
        has_balance = balance >= operation_value
        return has_balance, balance

    def update_balance(self, value):
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

    def insert_usdt(self, value):
        """
        Inserts USDT value into the database.
        Args:
            value (float): The USDT value to be inserted.
        """
        current_datetime = DateTimeUtils.get_datetime()
        self.data_manager.insert_usdt(value, current_datetime)
    def get_operation_value(self):
        """
        Calculates the operation value based on the current USDT balance.
        Returns:
            float: The operation value.
        """
        balance = self.data_manager.get_database_usdt_balance()
        operation_value = round(
            balance / (100 / settings.OPERATION_VALUE_PERCENTAGE), 2
        )
        if operation_value < 10:
            operation_value = 10
        return operation_value

