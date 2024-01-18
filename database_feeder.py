from datetime import datetime
from price_snapshot import PriceSnapshot



class DatabaseFeeder:
    def __init__(self, data_manager):
        self.data_manager = data_manager

    @classmethod
    def format_symbol(cls, symbol):
        """
        Adds "t" prefix to symbols starting with a digit.
        """
        return 't' + symbol if symbol[0].isdigit() else symbol

    def update_usdt_prices(self, usdt_pairs, timestamp):
        """
        Runs fetch, format, and store operations for all USDT pairs.
        """
        # pairs  = self.binance_api.fetch_usdt_pairs()
        # timestamp = int(time.time())

        for _, row in usdt_pairs.iterrows():
            symbol = self.format_symbol(row["symbol"])
            price = row['price']
            price_snapshot = PriceSnapshot(symbol, timestamp, price)
            self.data_manager.insert_price(price_snapshot)

        print(f"USD prices updated at {datetime.fromtimestamp(timestamp)}")
