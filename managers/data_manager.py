import sqlite3
import pandas as pd
from datetime import datetime, timedelta


class DataManager:
    """
    Manages data storage and retrieval from a SQLite database for asset and price information.

    Attributes:
        db_path (str): Path to the SQLite database file.
    """

    @staticmethod
    def _format_symbol(self, symbol):
        """
        Adds or removes "t" prefix to symbols starting with a digit.

        Args:
            symbol (str): The symbol to format.

        Returns:
            str: The formatted symbol.
        """
        if symbol[0].isdigit():
            return 't' + symbol
        if symbol[0] == 't':
            return symbol[1:]
        return symbol

    @staticmethod
    def _execute_sql(self, sql, error_message, params=None, table_name=None):
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
                    df = pd.read_sql(sql, conn).sort_index(ascending=False).reset_index(drop=True)
                    return df
                if params:
                    conn.execute(sql, params)
                else:
                    conn.execute(sql)
        except sqlite3.Error as e:
            print(f"{error_message}: {e}")

    @staticmethod
    def _fetch_one(self, sql, params, error_message):
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
            print(f"{error_message}: {e}")

    @staticmethod
    def _fetch_all(self, sql, error_message):
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
            print(f"{error_message}: {e}")

    @staticmethod
    def _create_coin_table(self, table_name):
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
        self._execute_sql(sql, f"Error creating coin table: {table_name}")

    @staticmethod
    def table_exists(self, table_name):
        """
        Checks if a table exists in the database.

        Args:
            table_name (str): Name of the table to check.

        Returns:
            bool: True if the table exists, otherwise False.
        """
        table_name = self._format_symbol(table_name)
        sql = f"SELECT name FROM sqlite_master WHERE type='table' AND name=?"
        return bool(self._fetch_one(sql, (table_name,), f"Error checking table existence for {table_name}"))

    @staticmethod
    def insert_price(self, price_snapshot):
        """
        Inserts price data for a USD symbol into the database.

        Args:
            price_snapshot (PriceSnapshot): Object containing price snapshot data.
        """
        table_name = self._format_symbol(price_snapshot.symbol)
        if not self.table_exists(table_name):
            self._create_coin_table(table_name)
        sql = f"INSERT INTO {table_name} (timestamp, price) VALUES (?, ?)"
        params = (price_snapshot.timestamp, price_snapshot.price)
        self._execute_sql(sql, f"Error inserting price for {table_name}", params)

        deletion_timestamp = int((datetime.fromtimestamp(price_snapshot.timestamp) - timedelta(days=1)).timestamp())
        self.delete_prices(table_name, deletion_timestamp)

    @staticmethod
    def delete_prices(self, table_name, timestamp):
        """
        Deletes price entries older than a specific timestamp for a USD symbol.

        Args:
            table_name (str): Name of the table to delete from.
            timestamp (int): Timestamp for deletion cutoff.
        """
        sql = f"DELETE FROM {table_name} WHERE timestamp < ?"
        params = (timestamp,)
        self._execute_sql(sql, f"Error deleting price for {table_name}", params)

    @staticmethod
    def _get_coin_prices_dataframe(self, table_name):
        """
        Retrieves price data for a USD symbol as a DataFrame.

        Args:
            table_name (str): Name of the table to fetch data from.

        Returns:
            pd.DataFrame: DataFrame containing price data.
        """
        table_name = self._format_symbol(table_name)
        if not self.table_exists(table_name):
            self._create_coin_table(table_name)
        sql = f"SELECT timestamp, price FROM {table_name}"
        df = self._execute_sql(sql, f"Error selecting from {table_name}", table_name=table_name)
        df['symbol'] = self._format_symbol(table_name)
        return df

    @staticmethod
    def get_all_coins_dataframes(self, usdt_pairs):
        """
        Retrieves price data for all USD symbols as a list of DataFrames.

        Args:
            usdt_pairs (pd.DataFrame): DataFrame containing USD symbol pairs.

        Returns:
            list: List of DataFrames containing price data for each symbol.
        """
        return [self._get_coin_prices_dataframe(row['symbol']) for _, row in usdt_pairs.iterrows()]

    @staticmethod
    def _create_assets_table(self):
        """
        Creates a table for storing asset information.
        """
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

    @staticmethod
    def insert_purchase(self, asset):
        """
        Inserts asset purchase information into the database.

        Args:
            asset (Asset): Object containing asset purchase data.
        """
        table_name = 'Assets'
        if not self.table_exists(table_name):
            self._create_assets_table()
        sql = f"INSERT INTO {table_name} (symbol, quantity, purchase_price, current_value, variation, purchase_datetime, highest_price, current_price, obs) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        params = (
        self._format_symbol(asset.symbol), asset.quantity, asset.purchase_price, asset.current_value, asset.variation,
        asset.purchase_datetime.strftime("%Y-%m-%d %H:%M:%S"), asset.highest_price, asset.current_price, asset.obs)
        self._execute_sql(sql, f"Error inserting price for {table_name}", params)

    @staticmethod
    def get_assets_dataframe(self):
        """
        Retrieves asset information from the database as a DataFrame.

        Returns:
            pd.DataFrame: DataFrame containing asset information.
        """
        table_name = 'Assets'
        if not self.table_exists(table_name):
            self._create_assets_table()
        sql = f"SELECT * FROM {table_name}"
        return self._execute_sql(sql, f"Error selecting from {table_name}", table_name=table_name)

    @staticmethod
    def update_asset(self, asset):
        """
        Updates asset information in the database.

        Args:
            asset (Asset): Object containing updated asset data.
        """
        table_name = 'Assets'
        sql = f"UPDATE {table_name} SET quantity = ?, current_value = ?, variation = ?, highest_price = ?, current_price = ? WHERE symbol = ?"
        params = (
        asset.quantity, asset.current_value, asset.variation, asset.highest_price, asset.current_price, asset.symbol)
        self._execute_sql(sql, f"Error inserting price for {asset.symbol}", params)

    @staticmethod
    def delete_from_assets(self, symbol):
        """
        Deletes asset information for a specific symbol from the database.

        Args:
            symbol (str): Symbol of the asset to delete.
        """
        table_name = 'Assets'
        sql = f"DELETE FROM {table_name} WHERE symbol == ?"
        params = (symbol,)
        self._execute_sql(sql, f"Error deleting price for {table_name}", params)
        self.drop_table(symbol)

    @staticmethod
    def drop_table(self, table_name):
        """
        Drops a table from the database.

        Args:
            table_name (str): Name of the table to drop.
        """
        sql = f"DROP TABLE {table_name}"
        self._execute_sql(sql, f'Error droping table {table_name}')

    @staticmethod
    def insert_usdt(self, value, current_datetime):
        """
        Inserts USDT balance information into the database.

        Args:
            value (float): Quantity of USDT.
            current_datetime (datetime): Datetime of the balance.
        """
        table_name = 'Assets'
        if not self.table_exists(table_name):
            self._create_assets_table()
        sql = f"INSERT INTO {table_name} (symbol, quantity, purchase_price, current_value, variation, purchase_datetime, highest_price, current_price) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        params = ('USDT', value, 1, value, 0, current_datetime.strftime("%Y-%m-%d %H:%M:%S"), 1, 1)
        self._execute_sql(sql, f"Error inserting USDT", params)

    @staticmethod
    def get_database_usdt_balance(self):
        """
        Retrieves the current USDT balance from the database.

        Returns:
            float: Current USDT balance.
        """
        table_name = 'Assets'
        sql = f"SELECT quantity FROM {table_name} WHERE symbol == ?"
        params = ('USDT',)
        balance = self._fetch_one(sql, params, f"Error selecting USDT")
        return balance[0] if balance else 0

    @staticmethod
    def update_usdt_balance(self, value):
        """
        Updates the USDT balance in the database.

        Args:
            value (float): New USDT balance.
        """
        table_name = 'Assets'
        sql = f"UPDATE {table_name} SET quantity = ?, current_value = ? WHERE symbol = ?"
        params = (value, value, 'USDT')
        self._execute_sql(sql, f"Error updating USDT", params)

    @staticmethod
    def get_total_asset_value(db_path):
        """
        Obtains the sum of all 'current_value' entries in the 'Assets' table.

        Args:
            db_path (str): Path to the SQLite database.

        Returns:
            float: Sum of all 'current_value' entries.
        """

        with sqlite3.connect(db_path) as conn:
            sql = "SELECT SUM(current_value) FROM Assets"
            result = conn.execute(sql).fetchone()
            return result[0] if result else 0

# DataManager('trading_info.db').drop_table('Assets')
# data_manager = DataManager('trading_info.db')
# print(data_manager._format_symbol('1INCHUSDT'))
