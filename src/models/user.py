from datetime import datetime
from hashlib import sha256
import bson


class UserSettings:
    """Represents user settings."""

    def __init__(
        self,
        testing: bool = False,
        operation_value_percentage: float = 5.0,
        maximum_operation_value: float = 100.0,
    ):
        """
        Initializes a UserSettings object.

        Args:
            testing (bool, optional): Indicates if the user is in testing mode. Defaults to False.
            operation_value_percentage (float, optional): The percentage of the USDT balance to be used for operations. Defaults to 5.0.
            maximum_operation_value (float, optional): The maximum value allowed for an operation. Defaults to 100.0.
        """
        self.testing = testing
        self.operation_value_percentage = operation_value_percentage
        self.maximum_operation_value = maximum_operation_value


class User:
    """Represents an user."""

    def __init__(
        self,
        login: str,
        name: str,
        api_key: str,
        secret_key: str,
        user_settings: UserSettings,
        usd_balance: float,
        created_at: datetime = datetime.now(),
        _id: bson.objectid.ObjectId | None = None,
        hashed_password: bytes = b"",
        str_password: str = "",
    ):
        """
        Initializes a User object.

        Args:
            login (str): The login name of the user.
            name (str): The name of the user.
            api_key (str): The API key of the user.
            secret_key (str): The secret key of the user.
            user_settings (UserSettings): The settings of the user.
            usd_balance (float): The USD balance of the user.
            created_at (datetime, optional): The creation datetime of the user. Defaults to datetime.now().
            _id (bson.objectid.ObjectId | None, optional): The ID of the user. Defaults to None.
            hashed_password (bytes, optional): The hashed password of the user. Defaults to b"".
            str_password (str, optional): The string password of the user. Defaults to "".

        Raises:
            ValueError: If neither hashed_password nor str_password is provided.
        """
        self._id = _id
        self.login = login
        self.name = name
        self.api_key = api_key
        self.secret_key = secret_key
        self.user_settings = user_settings
        self.usd_balance = usd_balance
        self.created_at = created_at
        if hashed_password:
            self.hashed_password = hashed_password
        elif str_password:
            self.hashed_password = self.encode_password(str_password)
        else:
            raise ValueError("A password must be provided")

    def encode_password(self, str_password):
        """Encodes the provided string password."""
        return sha256(str_password.encode("utf-8")).digest()

    def get_operation_value(self) -> float:
        """
        Calculates the operation value based on the current USDT balance.

        Returns:
            float: The operation value.
        """
        operation_value = round(
            self.usd_balance /
            (100 / self.user_settings.operation_value_percentage), 2
        )
        if operation_value < 10:
            operation_value = 10
        if operation_value > self.user_settings.maximum_operation_value:
            operation_value = self.user_settings.maximum_operation_value
        return operation_value

    def __str__(self) -> str:
        """Returns a string representation of the user."""
        return (
            f"User: {self.login} - Name: {self.name}\nUSD Balance: {self.usd_balance}"
        )


