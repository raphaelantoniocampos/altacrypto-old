import sqlite3
from datetime import datetime

import pandas as pd

from utils.datetime_utils import DateTimeUtils


class DatabaseManager:
    """
    Manages data storage and retrieval from a SQLite database for asset and price information.

    Attributes:
        db_path (str): Path to the SQLite database file.
    """

    def __init__(self, db_path: str):
        """
        Initializes a DataManager object with the path to the SQLite database.

        Args:
            db_path (str): Path to the SQLite database file.
        """
        self.db_path = db_path

    @staticmethod
    def _format_symbol(symbol: str) -> str:
        """
        Adds or removes "t" prefix to symbols starting with a digit.

        Args:
            symbol (str): The symbol to format.

        Returns:
            str: The formatted symbol.
        """
        if symbol[0].isdigit():
            return "t" + symbol
        if symbol[0] == "t":
            return symbol[1:]
        return symbol

    def execute_sql(
        self,
        sql: str,
        error_message: str,
        params: tuple | None = None,
        table_name: str | None = None,
    ) -> pd.DataFrame | None:
        """
        Executes an SQL statement with optional parameters.

        Args:
            sql (str): The SQL statement to execute.
            error_message (str): Error message to display in case of an exception.
            params (tuple, optional): Parameters for the SQL statement.
            table_name (str, optional): The name of the table to return as DataFrame.

        Returns:
            pd.DataFrame or None: DataFrame if table_name is provided, otherwise None.
        """
        try:
            with sqlite3.connect(self.db_path) as conn:
                if table_name:
                    dataframe = (
                        pd.read_sql(sql, conn)
                        .sort_index(ascending=False)
                        .reset_index(drop=True)
                    )
                    return dataframe
                if params:
                    conn.execute(sql, params)
                else:
                    conn.execute(sql)
        except sqlite3.Error as e:
            self.user_settings.logger.info(f"{error_message}: {e}")
        return None

    def _fetch_one(self, sql: str, params: tuple, error_message: str) -> tuple | None:
        """
        Fetches a single result from the database.

        Args:
            sql (str): The SQL statement to execute.
            params (tuple): Parameters for the SQL statement.
            error_message (str): Error message to display in case of an exception.

        Returns:
            tuple or None: Result of the query if found, otherwise None.
        """
        try:
            with sqlite3.connect(self.db_path) as conn:
                result = conn.execute(sql, params).fetchone()
                return result if result else None
        except sqlite3.Error as e:
            self.user_settings.logger.info(f"{error_message}: {e}")
        return None

    def _fetch_all(self, sql: str, error_message: str) -> list[tuple] | None:
        """
        Fetches all results from the database.

        Args:
            sql (str): The SQL statement to execute.
            error_message (str): Error message to display in case of an exception.

        Returns:
            list: List of tuples containing results from the query.
        """
        try:
            with sqlite3.connect(self.db_path) as conn:
                result = conn.execute(sql).fetchall()
                return result
        except sqlite3.Error as e:
            self.user_settings.logger.info(f"{error_message}: {e}")
        return None

    def table_exists(self, table_name: str) -> bool:
        """
        Checks if a table exists in the database.

        Args:
            table_name (str): Name of the table to check.

        Returns:
            bool: True if the table exists, otherwise False.
        """
        table_name = self._format_symbol(table_name)
        sql = "SELECT name FROM sqlite_master WHERE type='table' AND name=?"
        return bool(
            self._fetch_one(
                sql, (table_name,), f"Error checking table existence for {table_name}"
            )
        )

    def drop_table(self, table_name: str) -> None:
        """
        Drops a table from the database.

        Args:
            table_name (str): Name of the table to drop.
        """
        sql = f"DROP TABLE {table_name}"
        self._execute_sql(sql, f"Error droping table {table_name}")

    def insert_usdt(self, value: float, current_datetime: datetime) -> None:
        """
        Inserts USDT balance information into the database.

        Args:
            value (float): Quantity of USDT.
            current_datetime (datetime): Datetime of the balance.
        """
        table_name = "Assets"
        if not self.table_exists(table_name):
            self._create_assets_table()
        sql = f"INSERT INTO {table_name} (symbol, quantity, purchase_price, current_value, variation, purchase_datetime, highest_price, current_price) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        params = (
            "USDT",
            value,
            1,
            value,
            0,
            current_datetime.strftime("%Y-%m-%d %H:%M:%S"),
            1,
            1,
        )
        self._execute_sql(sql, "Error inserting USDT", params)

    def get_database_usdt_balance(self) -> float:
        """
        Retrieves the current USDT balance from the database.

        Returns:
            float: Current USDT balance.
        """
        table_name = "Assets"
        sql = f"SELECT quantity FROM {table_name} WHERE symbol == ?"
        params = ("USDT",)
        balance = self._fetch_one(sql, params, "Error selecting USDT")
        return balance[0] if balance else 0

    def update_usdt_balance(self, value: float) -> None:
        """
        Updates the USDT balance in the database.

        Args:
            value (float): New USDT balance.
        """
        table_name = "Assets"
        sql = (
            f"UPDATE {table_name} SET quantity = ?, current_value = ? WHERE symbol = ?"
        )
        params = (value, value, "USDT")
        self._execute_sql(sql, f"Error updating USDT", params)

    def get_total_asset_value(self) -> float:
        """
        Obtains the sum of all 'current_value' entries in the 'Assets' table.

        Returns:
            float: Sum of all 'current_value' entries.
        """

        with sqlite3.connect(self.db_path) as conn:
            sql = "SELECT SUM(current_value) FROM Assets"
            result = conn.execute(sql).fetchone()
            return result[0] if result else 0

    def feed_database(self, asset_pairs: pd.DataFrame) -> None:
        """
        Updates the database with price snapshots for asset pairs.

        Args:
            asset_pairs (pd.DataFrame): DataFrame containing asset pairs and their prices.
        """
        current_timestamp = DateTimeUtils.get_current_timestamp()
        for _, row in asset_pairs.iterrows():
            symbol = row["symbol"]
            price = row["price"]
            price_snapshot = PriceSnapshot(symbol, current_timestamp, price)
            self.insert_price(price_snapshot)

        self.user_settings.logger.info(
            f"USD prices updated at {datetime.fromtimestamp(current_timestamp)}"
        )
