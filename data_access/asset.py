import datetime
import pandas as pd

from data_access.database_manager import DatabaseManager


class Asset:
    """Represents an investment asset with details like symbol, quantity, prices, and performance."""

    def __init__(
        self,
        symbol: str,
        quantity: float,
        purchase_price: float,
        purchase_datetime: datetime.datetime,
        highest_price: float,
        current_price: float,
        obs: str | None = None,
    ):
        """
        Initializes an Asset object with data validation.

        Args:
            symbol: String representing the asset symbol.
            quantity: Integer number of units owned.
            purchase_price: Numeric purchase price per unit.
            purchase_datetime: Datetime object of the purchase date.
            highest_price: Numeric highest price reached since purchase.
            current_price: Numeric current market price per unit.
            obs: Optional string for any additional notes or observations.
        """
        self.symbol = symbol
        self.quantity = quantity
        self.purchase_price = purchase_price
        self.current_price = current_price
        self.current_value = self.calculate_current_value()
        self.variation = self.calculate_variation()
        self.purchase_datetime = purchase_datetime
        self.highest_price = highest_price
        self.obs = obs

    def calculate_current_value(self) -> float:
        """Calculates the current total value of the asset."""
        return round(self.current_price * self.quantity, 2)

    def calculate_variation(self) -> float:
        """Calculates the variation of the asset."""
        return round(
            (((self.current_price - self.purchase_price) / self.current_price) * 100), 2
        )

    def update_asset(self, new_price: float) -> None:
        """Updates the current price, the highest price if the new price is higher and recalculates variation."""
        self.current_price = new_price
        self.variation = self.calculate_variation()
        if self.current_price > self.highest_price:
            self.highest_price = self.current_price
        self.current_value = self.calculate_current_value()

    def calculate_profit_loss(self) -> float:
        """Calculates the profit or loss of the asset."""
        total_purchase_value = self.purchase_price * self.quantity
        return self.current_value - total_purchase_value

    def add_asset(
        self, database_manager: DatabaseManager, asset: "Asset", wallet_id: int
    ) -> None:
        """TODO: Document method"""
        table_name = "Assets"
        if not database_manager.table_exists(table_name):
            self.create_assets_table()
        sql = f"INSERT INTO {table_name} (wallet_id, symbol, quantity, purchase_price, current_price, current_value, variation, purchase_datetime,, highhest_price, obs) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        params = (
            wallet_id,
            self.database_manager.format_symbol(asset.symbol),
            asset.quantity,
            asset.purchase_price,
            asset.current_price,
            asset.current_value,
            asset.variation,
            asset.purchase_datetime.strftime("%Y-%m-%d %H:%M:%S"),
            asset.highest_price,
            asset.obs,
        )
        self.database_manager.execute_sql(
            sql,
            f"Error inserting purchase for {table_name} wallet_id: {wallet_id}",
            params,
        )

    def _create_assets_table(self) -> None:
        """
        Creates a table for storing assets.
        """
        table_name = "Assets"
        sql = f"""
        CREATE TABLE IF NOT EXISTS Assets (
            id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
            wallet_id INTEGER NOT NULL,
            quantity REAL NOT NULL,
            purchase_price REAL NOT NULL,
            current_value REAL NOT NULL,
            purchase_datetime TEXT NOT NULL,
            highest_price REAL NOT NULL,
            current_price REAL NOT NULL,
            obs TEXT,
            FOREIGN KEY (wallet_id) REFERENCES Wallets(id));
        """
        database_manager.execute_sql(sql, f"Error creating {table_name} table")

    @classmethod
    def from_series(cls, series: pd.Series) -> "Asset":
        """Creates an Asset object from a pandas Series."""
        symbol = series["symbol"]
        quantity = float(series["quantity"])
        purchase_price = float(series["purchase_price"])
        current_price = float(series["current_price"])
        purchase_datetime = datetime.datetime.strptime(
            series["purchase_datetime"], "%Y-%m-%d %H:%M:%S"
        )
        highest_price = float(series["highest_price"])
        obs = series["obs"]

        return cls(
            symbol,
            quantity,
            purchase_price,
            purchase_datetime,
            highest_price,
            current_price,
            obs,
        )

    def __str__(self) -> str:
        """Returns a string representation of the asset."""
        return (
            f"Symbol: {self.symbol}, Quantity: {self.quantity}, \n"
            f"Purchase Price: {self.purchase_price:.2f}, \n"
            f"Current Price: {self.current_price:.2f}, \n"
            f"Variation: {self.variation:.2f}%, \n"
            f"Highest Price: {self.highest_price:.2f}"
        )

