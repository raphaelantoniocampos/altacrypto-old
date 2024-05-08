from datetime import datetime
from hashlib import sha256
import bson

from models.user_settings import UserSettings


class User:
    """TODO: Document Class"""

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
        """TODO: Document method"""
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

