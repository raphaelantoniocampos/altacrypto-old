import time

import schedule
from managers.asset_analyzer import AssetAnalyzer
from managers.binance_manager import BinanceManager
from managers.balance_manager import BalanceManager
from managers.data_manager import DataManager
import utils.settings as settings
from utils.datetime_utils import DateTimeUtils


def main():
    binance_manager = BinanceManager(settings.API_KEY, settings.API_SECRET)
    data_manager = DataManager()
    balance_manager = BalanceManager(data_manager)
    asset_analyzer = AssetAnalyzer(binance_manager, data_manager, balance_manager)

    if settings.testing:
        if not data_manager.get_database_usdt_balance():
            current_datetime = DateTimeUtils.get_datetime()
            data_manager.insert_usdt(settings.TESTING_INITIAL_BALANCE, current_datetime)
        asset_analyzer.run()

    schedule.every(settings.EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(
        asset_analyzer.run
    )
    while True:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()

