from datetime import datetime
from datetime import timedelta

import pandas as pd

from data_access.database_manager import DatabaseManager
from utils.global_settings import GlobalSettings


class CryptoPryce:
    """TODO: Document class"""

    def __init__(self, symbol: str, timestamp: int, price: float):
        """TODO: Document method"""

        # Initialize attributes
        self.symbol = symbol.upper()
        self.timestamp = timestamp
        self.price = price
        self.datetime = datetime.fromtimestamp(timestamp)
        self.global_settings = GlobalSettings()
        self.database_manager = DatabaseManager(
            self.global_settings.crypto_prices_db_path
        )

    def _create_coin_table(self, table_name: str) -> None:
        """
        Creates a table for storing price data of a cryptocurrency.

        Args:
            table_name (str): Name of the table to create.
        """
        sql = f"""
        CREATE TABLE IF NOT EXISTS {table_name} (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            timestamp INTEGER NOT NULL,
            price REAL NOT NULL
        );
        """

        database_manager = DatabaseManager(
            self.global_settings.crypto_prices_db_path)
        database_manager.execute_sql(
            sql, f"Error creating coin table: {table_name}")

    def insert_price(self) -> None:
        """TODO: Document method"""
        table_name = self.database_manager.format_symbol(self.symbol)
        if not self.database_manager.table_exists(table_name):
            self._create_coin_table(table_name)
        sql = f"INSERT INTO {table_name} (timestamp, price) VALUES (?, ?)"
        params = (self.timestamp, self.price)
        self._execute_sql(
            sql, f"Error inserting price for {table_name}", params)
        deletion_timestamp = int(
            (datetime.fromtimestamp(self.timestamp) - timedelta(days=1)).timestamp()
        )
        self.delete_prices(table_name, deletion_timestamp)

    def delete_prices(self, table_name: str, timestamp: int) -> None:
        """
        Deletes price entries older than a specific timestamp for a USD symbol.

        Args:
            table_name (str): Name of the table to delete from.
            timestamp (int): Timestamp for deletion cutoff.
        """
        sql = f"DELETE FROM {table_name} WHERE timestamp < ?"
        params = (timestamp,)
        self.database_manager._execute_sql(
            sql, f"Error deleting price for {table_name}", params
        )

    def _get_coin_prices_dataframe(self, table_name: str) -> pd.DataFrame:
        """
        Retrieves price data for a USD symbol as a DataFrame.

        Args:
            table_name (str): Name of the table to fetch data from.

        Returns:
            pd.DataFrame: DataFrame containing price data.
        """
        table_name = self.database_manager.format_symbol(table_name)
        if not self.database_manager.table_exists(table_name):
            self._create_coin_table(table_name)
        sql = f"SELECT timestamp, price FROM {table_name}"
        df = self.database_manager._execute_sql(
            sql, f"Error selecting from {table_name}", table_name=table_name
        )
        if df is not None:
            df["symbol"] = self.database_manager.format_symbol(table_name)
        return df

    def get_all_coins_dataframes(self, usdt_pairs: pd.DataFrame) -> list[pd.DataFrame]:
        """
        Retrieves price data for all USD symbols as a list of DataFrames.

        Args:
            usdt_pairs (pd.DataFrame): DataFrame containing USD symbol pairs.

        Returns:
            list: List of DataFrames containing price data for each symbol.
        """
        return [
            self._get_coin_prices_dataframe(row["symbol"])
            for _, row in usdt_pairs.iterrows()
        ]

    def __str__(self) -> str:
        """Returns a string representation of the price snapshot."""
        return (
            f"Symbol: {self.symbol}, Timestamp: {self.timestamp}, "
            f"Price: {self.price:.2f}, Datetime: {self.datetime}"
        )

