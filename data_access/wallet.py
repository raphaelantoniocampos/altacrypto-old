import pandas as pd

from models.asset import Asset
from utils.global_settings import GlobalSettings
from data_access.database_manager import DatabaseManager


class Wallet:
    """Represents a user's wallet."""

    def __init__(self, balance: float):
        global_settings = GlobalSettings()
        self.database_manager = DatabaseManager(
            global_settings.user_data_db_path)

    def add_asset(self, asset: "Asset") -> None:
        """Adds an asset to the wallet."""
        self.assets.append(asset)

    def _create_wallet_table(self) -> None:
        """
        Creates a table for storing wallet information.
        """
        table_name = "Wallets"
        sql = f"""
        CREATE TABLE IF NOT EXISTS {table_name} (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            user_id INTEGER NOT NULL,
            symbol TEXT NOT NULL,
            quantity REAL NOT NULL,
            purchase_price REAL NOT NULL,
            current_value REAL NOT NULL,
            variation REAL NOT NULL,
            purchase_datetime TEXT NOT NULL,
            highest_price REAL NOT NULL,
            current_price REAL NOT NULL,
            obs TEXT,
            FOREIGN KEY (user_id) REFERENCES Users(id)
        );
        """
        self.database_manager.execute_sql(
            sql, f"Error creating {table_name} table")

    def insert_purchase(self, asset: Asset) -> None:
        """
        Inserts asset purchase information into the database.

        Args:
            asset (Asset): Object containing asset purchase data.
        """
        table_name = "Wallets"
        if not self.database_manager.table_exists(table_name):
            self._create_wallet_table()
        sql = f"INSERT INTO {table_name} (symbol, quantity, purchase_price, current_value, variation, purchase_datetime, highest_price, current_price, obs) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        params = (
            self.database_manager._format_symbol(asset.symbol),
            asset.quantity,
            asset.purchase_price,
            asset.current_value,
            asset.variation,
            asset.purchase_datetime.strftime("%Y-%m-%d %H:%M:%S"),
            asset.highest_price,
            asset.current_price,
            asset.obs,
        )
        self.database_manager.execute_sql(
            sql, f"Error inserting purchase for {table_name} id: {self.id}", params
        )

    def get_wallet_dataframe(self) -> pd.DataFrame:
        """
        Retrieves asset information from the database as a DataFrame.

        Returns:
            pd.DataFrame: DataFrame containing asset information.
        """
        table_name = "Assets"
        if not self.table_exists(table_name):
            self._create_assets_table()
        sql = f"SELECT * FROM {table_name}"
        return self._execute_sql(
            sql, f"Error selecting from {table_name}", table_name=table_name
        )

    def update_asset(self, asset: Asset) -> None:
        """
        Updates asset information in the database.

        Args:
            asset (Asset): Object containing updated asset data.
        """
        table_name = "Assets"
        sql = f"UPDATE {table_name} SET quantity = ?, current_value = ?, variation = ?, highest_price = ?, current_price = ? WHERE symbol = ?"
        params = (
            asset.quantity,
            asset.current_value,
            asset.variation,
            asset.highest_price,
            asset.current_price,
            asset.symbol,
        )
        self._execute_sql(
            sql, f"Error inserting price for {asset.symbol}", params)

    def delete_from_assets(self, symbol: str) -> None:
        """
        Deletes asset information for a specific symbol from the database.

        Args:
            symbol (str): Symbol of the asset to delete.
        """
        table_name = "Assets"
        sql = f"DELETE FROM {table_name} WHERE symbol == ?"
        params = (symbol,)
        self._execute_sql(
            sql, f"Error deleting price for {table_name}", params)
        self.drop_table(symbol)

    def __str__(self) -> str:
        """Returns a string representation of the wallet."""
        assets_info = "\n".join(str(asset) for asset in self.assets)
        return f"Wallet Balance: {self.balance:.2f}\nAssets:\n{assets_info}"
