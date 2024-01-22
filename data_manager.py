import sqlite3
import pandas as pd
from datetime import datetime, timedelta

class DataManager:
    def __init__(self, db_path):
        self.db_path = db_path

    @classmethod
    def format_symbol(cls, symbol):
        """
        Adds "t" prefix to symbols starting with a digit.
        """
        return 't' + symbol if symbol[0].isdigit() else symbol

    def _execute_sql(self, sql, error_message, params=None):
        """
        Executes an SQL statement with optional parameters.
        """
        try:
            with sqlite3.connect(self.db_path) as conn:
                if params:
                    conn.execute(sql, params)
                else:
                    conn.execute(sql)
        except sqlite3.Error as e:
            print(f"{error_message}: {e}")
    
    def _fetch_one(self, sql, params, error_message):
        try:
            with sqlite3.connect(self.db_path) as conn:
                result = conn.execute(sql, params).fetchone()
                return bool(result)
        except sqlite3.Error as e:
            print(f"{error_message}: {e}")
    
    def _fetch_all(self, sql, error_message):
        try:
            with sqlite3.connect(self.db_path) as conn:
                result = conn.execute(sql).fetchall()
                return result
        except sqlite3.Error as e:
            print(f"{error_message}: {e}")

    def create_coin_table(self, table_name):
        table_name = self.format_symbol(table_name)
        sql = f"""
        CREATE TABLE IF NOT EXISTS {table_name} (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            timestamp INTEGER NOT NULL,
            price REAL NOT NULL
        );
        """
        self._execute_sql(sql, f"Error creating {table_name} table")

    def table_exists(self, table_name):
        """
        Checks if a table exists in the database.
        """
        table_name = self.format_symbol(table_name)
        sql = f"SELECT name FROM sqlite_master WHERE type='table' AND name=?"
        return self._fetch_one(sql, (table_name,), f"Error checking table existence for {table_name}")

    def insert_price(self, price_snapshot):
        """
        Inserts price data for a USD symbol into the database.
        """
        table_name = self.format_symbol(price_snapshot.symbol)
        sql = f"INSERT INTO {table_name} (timestamp, price) VALUES (?, ?)"
        params = (price_snapshot.timestamp, price_snapshot.price)
        self._execute_sql(sql, f"Error inserting price for {table_name}", params)

        deletion_timestamp = int((datetime.fromtimestamp(price_snapshot.timestamp) - timedelta(days=1)).timestamp())
        self.delete_prices(table_name, deletion_timestamp)

    def delete_prices(self, table_name, timestamp):
        """
        Deletes a specific price entry for a USD symbol from its table.
        """
        sql = f"DELETE FROM {table_name} WHERE timestamp < ?"
        params = (timestamp,)
        self._execute_sql(sql, f"Error deleting price for {table_name}", params)

    def get_coin_prices_dataframe(self, table_name):
        table_name = self.format_symbol(table_name)
        sql = f"SELECT timestamp, price FROM {table_name}"
        try:
            with sqlite3.connect(self.db_path) as conn:
                    df = pd.read_sql(sql, conn).sort_index(ascending=False).reset_index(drop=True)
                    return df
        except sqlite3.Error as e:
            print(f"{e}")

    def get_distinct_timestamps(self, table_name):
        table_name = self.format_symbol(table_name)
        sql = f"SELECT DISTINCT timestamp FROM {table_name}"
        data = self._fetch_all(sql, f"Error selecting timestamps from {table_name}")
        return data
    