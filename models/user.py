import random
import string
import datetime
from models.wallet import Wallet
from models.asset import Asset
from utils.user_settings import UserSettings


class User:
    """Represents a user with name, API key, secret key, and wallet."""

    def __init__(
        self,
        login: str,
        password: str,
        name: str,
        api_key: str,
        secret_key: str,
        initial_balance: float,
    ):
        """
        Initializes a User object with name, API key, secret key, and a wallet.

        Args:
            login: User's login.
            password: User's password.
            name: User's name.
            api_key: User's API key.
            secret_key: User's secret key.
            initial_balance: Initial balance of the user's wallet.
        """
        self.id = self.generate_id()
        self.login = login
        self.password = password
        self.name = name
        self.api_key = api_key
        self.secret_key = secret_key
        self.wallet = Wallet(initial_balance)
        self.user_settings = UserSettings()

    def generate_id(self) -> str:
        """Generates a random ID for the user."""
        random_id = "".join(random.choices(string.ascii_uppercase + string.digits, k=8))
        return random_id

    def __str__(self) -> str:
        """Returns a string representation of the user."""
        return f"ID: {self.id}, Name: {self.name}, API Key: {self.api_key}, Secret Key: {self.secret_key}, {self.wallet}"

