import schedule
import time
import csv
import re

import pandas as pd
from datetime import datetime
import logging


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def main():
    binance_api = BinanceAPI(API_KEY, API_SECRET)
    data_manager = DataManager(DB_PATH)
    database_feeder = DatabaseFeeder(data_manager)

    if not data_manager.get_usdt_balance():
        insert_usdt(TESTING_INITIAL_BALANCE)

    fetch_and_analyze_assets_data()

    schedule.every(EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(fetch_and_analyze_assets_data)
    while True:
        schedule.run_pending()
        time.sleep(1)

if __name__ == "__main__":
    main()

    
