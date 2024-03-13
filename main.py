import time

import schedule
from managers.asset_analyzer import AssetAnalyzer
from managers.binance_manager import BinanceManager
from managers.data_manager import DataManager
from managers.balance_manager import BalanceManager
import utils.settings as settings


def main():
    binance_manager = BinanceManager(settings.API_KEY, settings.API_SECRET)
    data_manager = DataManager()
    asset_analyzer = AssetAnalyzer(binance_manager, data_manager)

    if settings.testing:
        balance_manager = BalanceManager(data_manager)
        if not balance_manager.has_balance[1]:
            data_manager.insert_usdt(settings.TESTING_INITIAL_BALANCE)
        asset_analyzer.run()

    schedule.every(settings.EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(
        asset_analyzer.run
    )
    while True:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()
