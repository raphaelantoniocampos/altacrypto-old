from datetime import datetime
from hashlib import sha256
from typing import List

from models.user_settings import UserSettings
from models.asset import Asset


class User:
    """TODO: Document Class"""

    def __init__(
        self,
        login: str,
        name: str,
        tier: str,
        api_key: str,
        secret_key: str,
        user_settings: UserSettings,
        assets: List[Asset],
        created_at: datetime = datetime.now(),
        id="",
        hashed_password: bytes = b"",
        str_password: str = "",
    ):
        self.id = id
        self.login = login
        self.name = name
        self.tier = tier
        self.api_key = api_key
        self.secret_key = secret_key
        self.user_settings = UserSettings()
        self.assets = assets
        self.created_at = created_at
        if hashed_password:
            self.hashed_password = hashed_password
        elif str_password:
            self.hashed_password = self.encode_password(str_password)
        else:
            raise ValueError("A password must be provided")

        """
        if self.user_settings.testing:
            if not data_manager.get_database_usdt_balance():
                current_datetime = DateTimeUtils.get_datetime()
                data_manager.insert_usdt(
                    user_settings.testing_initial_balance, current_datetime
                )
                logger.info("Initializing testing mode")
                logger.info(
                    f"Inserting USDT: {user_settings.testing_initial_balance}")
        """

    def encode_password(self, str_password):
        return sha256(str_password.encode("utf-8")).digest()

    def __str__(self) -> str:
        """Returns a string representation of the user."""
        return f"User: {self.login} - Name: {self.name} - Tier: {self.tier}"
