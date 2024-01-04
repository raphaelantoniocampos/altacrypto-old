import os
from datetime import datetime, timedelta
import time
from dotenv import load_dotenv

from data_manager import DataManager
from binance_api import BinanceAPI
from price_snapshot import PriceSnapshot

load_dotenv()

API_KEY = os.environ.get('BINANCE_API_KEY_TEST')
API_SECRET = os.environ.get('BINANCE_API_SECRET_TEST')

DB_PATH  = "coin_prices.db"

class DatabaseFeeder:
    def __init__(self):
        self.data_manager = DataManager(DB_PATH)
        self.binance_api = BinanceAPI(API_KEY, API_SECRET)

    @classmethod 
    def format_symbol(cls, symbol):
        """
        Adds "t" prefix to symbols starting with a digit.
        """
        return 't' + symbol if symbol[0].isdigit() else symbol

    def update_usdt_prices(self):
        """
        Runs fetch, format, and store operations for all USDT pairs.
        """
        pairs  = self.binance_api.fetch_usdt_pairs()
        timestamp = int(time.time())

        for _, row in pairs.iterrows():
            symbol = self.format_symbol(row["symbol"])

            price_snapshot = PriceSnapshot(symbol, timestamp, row["price"])
            self.data_manager.create_table(symbol)
            self.data_manager.insert_price(price_snapshot)

        print(f"USD prices updated at {datetime.fromtimestamp(timestamp)}")

    def run(self):
        self.update_usdt_prices()

if __name__ == "__main__":
    database_feeder = DatabaseFeeder()      
    database_feeder.run()
