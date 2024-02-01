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
                return result if result else None 
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
        return bool(self._fetch_one(sql, (table_name,), f"Error checking table existence for {table_name}"))

    def insert_price(self, price_snapshot):
        """
        Inserts price data for a USD symbol into the database.
        """
        table_name = self._format_symbol(price_snapshot.symbol)
        if not self.table_exists(table_name):
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
        if not self.table_exists(table_name):
            self._create_coin_table(table_name)
        sql = f"SELECT timestamp, price FROM {table_name}"
        df = self._execute_sql(sql, f"Error selecting from {table_name}", table_name= table_name)
        df['symbol'] = table_name
        return df

    def get_all_coins_dataframes(self, usdt_pairs):
        return [self.get_coin_prices_dataframe(row['symbol']) for _, row in usdt_pairs.iterrows()]

    def _create_assets_table(self):
        table_name = 'Assets'
        sql = f"""
        CREATE TABLE IF NOT EXISTS {table_name} (
            symbol TEXT PRIMARY KEY NOT NULL,
            quantity REAL NOT NULL,
            purchase_price REAL NOT NULL,
            current_value REAL NOT NULL,
            variation REAL NOT NULL,
            purchase_datetime TEXT NOT NULL,
            highest_price REAL NOT NULL,
            current_price REAL NOT NULL,
            obs TEXT
        );
        """
        self._execute_sql(sql, f"Error creating {table_name} table")
    
    def insert_purchase(self, asset):
        table_name = 'Assets'
        if not self.table_exists(table_name):
            self._create_assets_table()
        sql = f"INSERT INTO {table_name} (symbol, quantity, purchase_price, current_value, variation, purchase_datetime, highest_price, current_price, obs) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        params = (self._format_symbol(asset.symbol), asset.quantity, asset.purchase_price, asset.current_value, asset.variation, asset.purchase_datetime.strftime("%Y-%m-%d %H:%M:%S"), asset.highest_price, asset.current_price, asset.obs)
        self._execute_sql(sql, f"Error inserting price for {table_name}", params)

    def get_assets_dataframe(self):
        table_name = 'Assets'
        if not self.table_exists(table_name):
            self._create_assets_table()
        sql = f"SELECT * FROM {table_name}"
        return self._execute_sql(sql, f"Error selecting from {table_name}", table_name= table_name)

    def update_asset(self, asset):
        table_name = 'Assets'
        sql = f"UPDATE {table_name} SET quantity = ?, current_value = ?, variation = ?, highest_price = ?, current_price = ? WHERE symbol = ?"
        params = (asset.quantity, asset.current_value, asset.variation, asset.highest_price, asset.current_price, asset.symbol)
        self._execute_sql(sql, f"Error inserting price for {asset.symbol}", params)

    def delete_from_assets(self, symbol):
        table_name = 'Assets'
        sql = f"DELETE FROM {table_name} WHERE symbol == ?"
        params = (symbol,)
        self._execute_sql(sql, f"Error deleting price for {table_name}", params)
        self.drop_table(symbol)

    def drop_table(self, table_name):
        sql = f"DROP TABLE {table_name}"
        self._execute_sql(sql, f'Error droping table {table_name}')

    def insert_usdt(self, value, current_datetime):
        table_name = 'Assets'
        if not self.table_exists(table_name):
            self._create_assets_table()
        sql = f"INSERT INTO {table_name} (symbol, quantity, purchase_price, current_value, variation, purchase_datetime, highest_price, current_price) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        params = ('USDT', value, 1, value, 0, current_datetime.strftime("%Y-%m-%d %H:%M:%S"), 1, 1)
        self._execute_sql(sql, f"Error inserting USDT", params)

    def get_usdt_balance(self):
        table_name = 'Assets'
        sql = f"SELECT quantity FROM {table_name} WHERE symbol == ?"
        params = ('USDT',)
        balance = self._fetch_one(sql, params, f"Error selecting USDT")
        return balance[0] if balance else 0 

    def update_usdt_balance(self, value):
        table_name = 'Assets'
        sql = f"UPDATE {table_name} SET quantity = ?, current_value = ? WHERE symbol = ?"
        params = (value, value, 'USDT')
        self._execute_sql(sql, f"Error updating USDT", params)

    def get_total_asset_value(db_path):
        """
        Obtém a soma de todos os 'current_value' na tabela 'Assets'.

        Args:
            db_path: Caminho para o banco de dados SQLite.

        Returns:
            A soma de todos os 'current_value'.
        """

        with sqlite3.connect(db_path) as conn:
            sql = "SELECT SUM(current_value) FROM Assets"
            result = conn.execute(sql).fetchone()
            return result[0] if result else 0


#DataManager('trading_info.db').drop_table('Assets')
#data_manager = DataManager('trading_info.db')
#print(data_manager._format_symbol('1INCHUSDT'))