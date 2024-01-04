import sqlite3
from datetime import datetime, timedelta

class DataManager:
    def __init__(self, db_path):
        self.db_path = db_path

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
        """
        Executes a SELECT SQL statement and returns a boolean indicating whether a record exists.
        """
        try:
            with sqlite3.connect(self.db_path) as conn:
                result = conn.execute(sql, params).fetchone()
                return bool(result)
        except sqlite3.Error as e:
            print(f"{error_message}: {e}")

    def create_table(self, table_name):
        """
        Creates a table for storing price data of a USD symbol.
        """
        sql = f"""
        CREATE TABLE IF NOT EXISTS {table_name} (
            timestamp INTEGER PRIMARY KEY,
            price REAL NOT NULL
        );
        """
        self._execute_sql(sql, f"Error creating table {table_name}")

    def table_exists(self, table_name):
        """
        Checks if a table exists in the database.
        """
        sql = f"SELECT name FROM sqlite_master WHERE type='table' AND name=?"
        return self._fetch_one(sql, (table_name,), f"Error checking table existence for {table_name}")

    def insert_price(self, price_snapshot):
        """
        Inserts price data for a USD symbol into its table.
        """
        sql = f"INSERT INTO {price_snapshot.symbol} (timestamp, price) VALUES (?, ?)"
        params = (price_snapshot.timestamp, price_snapshot.price)
        self._execute_sql(sql, f"Error inserting price for {price_snapshot.symbol}", params)

        deletion_timestamp = int((datetime.fromtimestamp(price_snapshot.timestamp) - timedelta(days=1)).timestamp())
        self.delete_price(price_snapshot.symbol, deletion_timestamp)

    def delete_price(self, symbol, timestamp):
        """
        Deletes a specific price entry for a USD symbol from its table.
        """
        sql = f"DELETE FROM {symbol} WHERE timestamp < ?"
        params = (timestamp,)
        self._execute_sql(sql, f"Error deleting price for {symbol}", params)

    
