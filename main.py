import time

import schedule

import utils.settings as settings
from managers.asset_analyzer import AssetAnalyzer
from managers.balance_manager import BalanceManager
from managers.binance_manager import BinanceManager
from managers.data_manager import DataManager
from utils.datetime_utils import DateTimeUtils
from utils.quick_operations import refresh_database

# refresh_database()

def main():
    settings.logger.info("Starting bot")
    binance_manager = BinanceManager(settings.API_KEY, settings.API_SECRET)
    data_manager = DataManager()
    balance_manager = BalanceManager(data_manager)
    asset_analyzer = AssetAnalyzer(binance_manager, data_manager, balance_manager)

    if settings.testing:
        if not data_manager.get_database_usdt_balance():
            current_datetime = DateTimeUtils.get_datetime()
            data_manager.insert_usdt(settings.TESTING_INITIAL_BALANCE, current_datetime)
            settings.logger.info("Initializing testing mode")
            settings.logger.info(f"Inserting USDT: {settings.TESTING_INITIAL_BALANCE}")
        asset_analyzer.run()

    schedule.every(settings.EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(
        asset_analyzer.run
    )
    while True:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()

