import schedule
import time
import csv
import re

import pandas as pd
from datetime import datetime


import models
import trading
import utils.settings



def main():
    '''
    binance_manager = BinanceManager(API_KEY, API_SECRET)
    data_manager = DataManager(DB_PATH)
    database_feeder = DatabaseFeeder(data_manager)
    '''

    schedule.every(EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(trading.managers.asset_analyzer.run)
    while True:
        schedule.run_pending()
        time.sleep(1)

if __name__ == "__main__":
    main()
