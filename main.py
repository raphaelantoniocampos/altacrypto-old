import schedule
import time
import os

from database_feeder import DatabaseFeeder
from binance_api import BinanceAPI
from price_snapshot import PriceSnapshot
from data_manager import DataManager

from dotenv import load_dotenv

load_dotenv()

API_KEY = os.environ.get('BINANCE_API_KEY_TEST')
API_SECRET = os.environ.get('BINANCE_API_SECRET_TEST')

DB_PATH  = "trading_info.db"

def analyze(usdt_pairs, timestamp):
    pass

def run():
    usdt_pairs = binance_api.fetch_usdt_pairs()
    timestamp = int(time.time())
    database_feeder.update_usdt_prices(usdt_pairs, timestamp)
    print(data_manager.get_coin_prices_dataframe().iloc[::-1, ::-1])


if __name__ == "__main__":
    data_manager = DataManager(DB_PATH)
    database_feeder = DatabaseFeeder(data_manager)
    binance_api = BinanceAPI(API_KEY, API_SECRET)

    schedule.every(5).minutes.at(":00").do(run)
    while True:
        schedule.run_pending()
        time.sleep(1)
