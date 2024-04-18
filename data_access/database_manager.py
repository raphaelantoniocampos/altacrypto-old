import sqlite3
from datetime import datetime
import inspect
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
    def format_symbol(symbol: str) -> str:
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

    def fetch_one(self, sql: str, params: tuple, error_message: str) -> tuple | None:
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

    def fetch_all(self, sql: str, error_message: str) -> list[tuple] | None:
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
        table_name = self.format_symbol(table_name)
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

