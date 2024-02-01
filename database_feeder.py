from datetime import datetime
from models.price_snapshot import PriceSnapshot

class DatabaseFeeder:
    def __init__(self, data_manager):
        self.data_manager = data_manager

    def update_database(self, asset_pairs, current_timestamp):
        """
        Runs fetch, format, and store operations for all USDT pairs.
        """
        for _, row in asset_pairs.iterrows():
            symbol = row["symbol"]
            price = row['price']
            price_snapshot = PriceSnapshot(symbol, current_timestamp, price)
            self.data_manager.insert_price(price_snapshot)

        print(f"USD prices updated at {datetime.fromtimestamp(current_timestamp)}")
