from datetime import datetime
from price_snapshot import PriceSnapshot



class DatabaseFeeder:
    def __init__(self, data_manager):
        self.data_manager = data_manager

    def update_usdt_prices(self, usdt_pairs, timestamp):
        """
        Runs fetch, format, and store operations for all USDT pairs.
        """
        for _, row in usdt_pairs.iterrows():
            symbol = row["symbol"]
            self.data_manager.create_coin_table(symbol)
            price = row['price']
            price_snapshot = PriceSnapshot(symbol, timestamp, price)
            self.data_manager.insert_price(price_snapshot)

        print(f"USD prices updated at {datetime.fromtimestamp(timestamp)}")
