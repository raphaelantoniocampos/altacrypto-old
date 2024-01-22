import schedule
import time
import os

from database_feeder import DatabaseFeeder
from binance_api import BinanceAPI
from data_manager import DataManager

from dotenv import load_dotenv


import pprint

load_dotenv()

API_KEY = os.environ.get('BINANCE_API_KEY_TEST')
API_SECRET = os.environ.get('BINANCE_API_SECRET_TEST')

DB_PATH  = "trading_info.db"

def analyze(usdt_pairs, timestamp):
    pass

def run(data_manager):
    usdt_pairs = binance_api.fetch_usdt_pairs()
    timestamp = int(time.time())
    database_feeder.update_usdt_prices(usdt_pairs, timestamp)

    intervals = [1, 2, 3, 6, 12, 24, 48, 72, 144]
    timestamps = [timestamp[0] for timestamp in data_manager.get_distinct_timestamps(usdt_pairs.iloc[0]['symbol'])]
    # print(timestamps)

    '''for _, row in usdt_pairs.iterrows():
        df = data_manager.get_coin_prices_dataframe(row['symbol'])
        for interval in intervals:
            if len(df) > interval:
                print(df)
                
        print(row['symbol'])
        print(df)'''
        



    



if __name__ == "__main__":
    data_manager = DataManager(DB_PATH)
    database_feeder = DatabaseFeeder(data_manager)
    binance_api = BinanceAPI(API_KEY, API_SECRET)

    run(data_manager)

    schedule.every(5).minutes.at(":00").do(run)
    while True:
        schedule.run_pending()
        time.sleep(1)
