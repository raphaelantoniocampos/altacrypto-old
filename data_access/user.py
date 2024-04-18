import random
import string
from utils.user_settings import UserSettings
from data_access.wallet import Wallet
from data_access.database_manager import DatabaseManager


class User:
    """TODO: Document Class"""

    def __init__(
        self,
        login: str,
        password: str,
        name: str,
        api_key: str,
        secret_key: str,
        initial_balance: float,
    ):
        self.id = self._generate_id()
        self.login = login
        self.password = password
        self.name = name
        self.api_key = api_key
        self.secret_key = secret_key
        self.wallet = Wallet()
        self.user_settings = UserSettings()
        self.database_manager = DatabaseManager(
            self.global_settings.user_data_db_path)

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

    def _generate_id(self) -> str:
        """Generates a random ID for the user."""
        random_id = "".join(random.choices(
            string.ascii_uppercase + string.digits, k=8))
        return random_id

    @staticmethod
    def get_all_users(database_manager: DatabaseManager):
        table_name = "Users"
        sql = f"SELECT id FROM Users"
        result = database_manager.fetch_all(sql, "Error fetching all users")
        return [row[0] for row in result] if result else []

    @staticmethod
    def create_user_table(database_manager: DatabaseManager) -> None:
        """
        Creates a table for storing user information.
        """
        table_name = "Users"
        sql = f"""
        CREATE TABLE IF NOT EXISTS {table_name} (
            id INTEGER PRIMARY KEY,
            login TEXT NOT NULL,
            password TEXT NOT NULL,
            name TEXT NOT NULL,
            api_key TEXT NOT NULL,
            secret_key TEXT NOT NULL,
            initial_balance REAL
        );
        """
        database_manager.execute_sql(sql, f"Error creating {table_name} table")

    def __str__(self) -> str:
        """Returns a string representation of the user."""
        return f"ID: {self.id}, Name: {self.name}, API Key: {self.api_key}, Secret Key: {self.secret_key}, {self.wallet}"

