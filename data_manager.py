import sqlite3

from price_snapshot import PriceSnapshot

class DataManager:
    def __init__(self, db_path):
        self.db_path = db_path

    def create_table(self, table_name):
        """
        Creates a table for storing price data of a USD symbol.
        """
        sql = f"""
        CREATE TABLE {table_name} (
            time TEXT PRIMARY KEY,
            date TEXT,
            price REAL
        );
        """
        try:
            with sqlite3.connect(self.db_path) as conn:
                conn.execute(sql)
        except sqlite3.Error as e:
            print(f"Error creating table {table_name}: {e}")

    def table_exists(self, table_name):
        """
        Checks if a table exists in the database.
        """
        sql = f"SELECT name FROM sqlite_master WHERE type='table' AND name='{table_name}'"
        try:
            with sqlite3.connect(self.db_path) as conn:
                return bool(conn.execute(sql).fetchone())
        except sqlite3.Error as e:
            print(f"Error selecting from database: {e}")

    def insert_price(self, price_snapshot):
        """
        Inserts price data for a USD symbol into its table.
        """
        sql = f"INSERT INTO {price_snapshot.symbol} (time, date, price) VALUES (?, ?, ?)"
        try:
            with sqlite3.connect(self.db_path) as conn:
                conn.execute(sql, (price_snapshot.time, price_snapshot.date, price_snapshot.price))
        except sqlite3.Error as e:
            print(f"Error inserting price for {price_snapshot.symbol}: {e}")

    def update_price(self, price_snapshot):
        """
        Updates the price data for a USD symbol.
        """
        sql = f"UPDATE {price_snapshot.symbol} SET price = ?, date = ? WHERE time = ?"
        try:
            with sqlite3.connect(self.db_path) as conn:
                conn.execute(sql, (price_snapshot.price, price_snapshot.date, price_snapshot.time))
        except sqlite3.Error as e:
            print(f"Error updating price for {price_snapshot.symbol}: {e}")

    def price_exists(self, price_snapshot):
        """
        Check for existing price record for the given time
        """
        sql = f"SELECT * FROM {price_snapshot.symbol} WHERE time = '{price_snapshot.time}'"
        try:
            with sqlite3.connect(self.db_path) as conn:
                cursor = conn.cursor()
                cursor.execute(sql)
                return cursor.fetchone()
        except sqlite3.Error as e:
            print(f"Error selecting {price_snapshot.time} from {price_snapshot.symbol}: {e}")
            return None

    
