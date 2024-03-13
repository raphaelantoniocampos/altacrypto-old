class BalanceManager:
    @staticmethod
    def has_balance(operation_value):
        """
        Check if there is enough balance to perform an operation.

        Args:
            operation_value (float): The value of the operation.

        Returns:
            bool: True if there is enough balance, False otherwise.
        """
        balance = data_manager.get_usdt_balance()
        has_balance = balance >= operation_value
        return (has_balance, balance)

    @staticmethod
    def update_balance(value):
        """
        Update the balance in the database.

        Args:
            value (float): The value to be added or subtracted from the balance.

        Returns:
            float: The new balance after the update.
        """
        balance = data_manager.get_usdt_balance()
        new_balance = round((balance + value), 2)
        data_manager.update_usdt_balance(new_balance)
        return new_balance

    @staticmethod
    def insert_usdt(value):
        """
        Inserts USDT value into the database.

        Args:
            value (float): The USDT value to be inserted.
        """
        current_datetime = get_datetime()
        data_manager.insert_usdt(value, current_datetime)

    @staticmethod
    def get_operation_value():
        """
        Calculates the operation value based on the current USDT balance.

        Returns:
            float: The operation value.
        """
        balance = data_manager.get_usdt_balance()
        operation_value = round(
            balance / (100 / OPERATION_VALUE_PERCENTAGE), 2)
        if operation_value < 10:
            operation_value = 10
        return operation_value
