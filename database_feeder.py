import os
from datetime import datetime
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

    def format_symbol(self, symbol):
        """
        Adds "t" prefix to symbols starting with a digit.
        """
        return 't' + symbol if symbol[0].isdigit() else symbol

    def update_usdt_prices(self):
        """
        Runs fetch, format, and store operations for all USDT pairs.
        """
        pairs  = self.binance_api.fetch_usdt_pairs()
        time_str = datetime.now().strftime('%H:%M')
        date = datetime.now().strftime('%d/%m/%Y')

        for _, row in pairs.iterrows():
            symbol = self.format_symbol(row["symbol"])

            price_snapshot = PriceSnapshot(symbol, time_str, date, row["price"])
            if not self.data_manager.table_exists(symbol):
                self.data_manager.create_table(symbol)

            if not self.data_manager.price_exists(price_snapshot):
                self.data_manager.insert_price(price_snapshot)
            else:
                self.data_manager.update_price(price_snapshot)

        print(f"USD prices updated at {date} - {time_str}")

    def run(self):
        self.update_usdt_prices()

if __name__ == "__main__":
    database_feeder = DatabaseFeeder()
    database_feeder.run()
