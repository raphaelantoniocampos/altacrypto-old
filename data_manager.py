import sqlite3
import pandas as pd
from datetime import datetime, timedelta

class DataManager:
    def __init__(self, db_path):
        self.db_path = db_path

    def _format_symbol(self, symbol):
        """
        Adds or remove "t" prefix to symbols starting with a digit.
        """
        if symbol[0].isdigit():
            return 't' + symbol
        if symbol[0] == 't':
            return symbol[1:]
        return symbol

    def _execute_sql(self, sql, error_message, params=None, table_name = None):
        """
        Executes an SQL statement with optional parameters.
        """
        try:
            with sqlite3.connect(self.db_path) as conn:
                if table_name:
                    df = pd.read_sql(sql, conn).sort_index(ascending=False).reset_index(drop=True)
                    return df
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

    def _create_coin_table(self, table_name):
        sql = f"""
        CREATE TABLE IF NOT EXISTS {table_name} (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            timestamp INTEGER NOT NULL,
            price REAL NOT NULL
        );
        """
        self._execute_sql(sql, f"Error creating coin table: {table_name}")

    def table_exists(self, table_name):
        """
        Checks if a table exists in the database.
        """
        table_name = self._format_symbol(table_name)
        sql = f"SELECT name FROM sqlite_master WHERE type='table' AND name=?"
        return self._fetch_one(sql, (table_name,), f"Error checking table existence for {table_name}")

    def insert_price(self, price_snapshot):
        """
        Inserts price data for a USD symbol into the database.
        """
        table_name = self._format_symbol(price_snapshot.symbol)
        self._create_coin_table(table_name)
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
        table_name = self._format_symbol(table_name)
        self._create_coin_table(table_name)
        sql = f"SELECT timestamp, price FROM {table_name}"
        df = self._execute_sql(sql, f"Error selecting from {table_name}", table_name= table_name)
        df['symbol'] = table_name
        return df

    def get_all_coins_dataframes(self, usdt_pairs):
        return [self.get_coin_prices_dataframe(row['symbol']) for _, row in usdt_pairs.iterrows()]

    def _create_assets_table(self, table_name):
        sql = f"""
        CREATE TABLE IF NOT EXISTS {table_name} (
            symbol TEXT PRIMARY KEY NOT NULL,
            purchase_datetime TEXT NOT NULL,
            purchase_price REAL NOT NULL,
            highest_price REAL NOT NULL,
            current_price REAL NOT NULL,
            variation REAL NOT NULL
        );
        """
        self._execute_sql(sql, f"Error creating {table_name} table")
    
    def insert_purchase(self, asset):
        table_name = 'Assets'
        self._create_assets_table(table_name)
        sql = f"INSERT INTO {table_name} (symbol, purchase_datetime, purchase_price, highest_price, current_price, variation) VALUES (?, ?, ?, ?, ?, ?)"
        params = (asset.symbol, asset.purchase_datetime.strftime("%Y-%m-%d %H:%M:%S"), asset.current_price, asset.current_price, asset.current_price, 0)
        self._execute_sql(sql, f"Error inserting price for {table_name}", params)

    def get_assets_dataframe(self):
        table_name = 'Assets'
        self._create_assets_table(table_name)
        sql = f"SELECT * FROM {table_name}"
        return self._execute_sql(sql, f"Error selecting from {table_name}", table_name= table_name)

    def update_asset(self, asset):
        table_name = 'Assets'
        sql = f"UPDATE {table_name} SET current_price = ?, highest_price = ?, variation = ? WHERE symbol = ?"
        params = (asset.current_price, asset.highest_price, asset.variation, asset.symbol)
        self._execute_sql(sql, f"Error inserting price for {table_name}", params)

    def delete_from_assets(self, symbol):
        table_name = 'Assets'
        sql = f"DELETE FROM {table_name} WHERE symbol == ?"
        params = (symbol,)
        self._execute_sql(sql, f"Error deleting price for {table_name}", params)
        self.drop_table(symbol)

    def drop_table(self, table_name):
        sql = f"DROP TABLE {table_name}"
        self._execute_sql(sql, f'Error droping table {table_name}')

# DataManager('trading_info.db').drop_table('Assets')
#data_manager = DataManager('trading_info.db')
#print(data_manager._format_symbol('1INCHUSDT'))