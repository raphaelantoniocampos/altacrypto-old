from datetime import datetime
from models.price_snapshot import PriceSnapshot

class DatabaseFeeder:
    """
    Updates the database with price snapshots for asset pairs.

    Attributes:
        data_manager (DataManager): The DataManager object for database interaction.
    """
    def __init__(self, data_manager):
        """
        Initializes a DatabaseFeeder object with a DataManager.

        Args:
            data_manager (DataManager): The DataManager object for database interaction.
        """
        self.data_manager = data_manager

    def update_database(self, asset_pairs, current_timestamp):
        """
        Updates the database with price snapshots for asset pairs.

        Args:
            asset_pairs (pd.DataFrame): DataFrame containing asset pairs and their prices.
            current_timestamp (int): Current timestamp for the price snapshots.
        """
        for _, row in asset_pairs.iterrows():
            symbol = row["symbol"]
            price = row['price']
            price_snapshot = PriceSnapshot(symbol, current_timestamp, price)
            self.data_manager.insert_price(price_snapshot)

        print(f"USD prices updated at {datetime.fromtimestamp(current_timestamp)}")
