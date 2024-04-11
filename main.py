import time

import schedule

from managers.asset_analyzer import AssetAnalyzer
from managers.balance_manager import BalanceManager
from managers.binance_manager import BinanceManager
from managers.data_manager import DataManager
from utils.datetime_utils import DateTimeUtils
from utils.quick_operations import refresh_database
from utils.user_settings import UserSettings
# refresh_database()


def main():
    user_settings = UserSettings(testing=True)
    user_settings.logger.info("Starting bot")
    binance_manager = BinanceManager(
        user_settings, user_settings.api_key, user_settings.api_secret
    )
    data_manager = DataManager(user_settings.db_path, user_settings)
    balance_manager = BalanceManager(user_settings, data_manager)
    asset_analyzer = AssetAnalyzer(
        user_settings, binance_manager, data_manager, balance_manager
    )

    if user_settings.testing:
        if not data_manager.get_database_usdt_balance():
            current_datetime = DateTimeUtils.get_datetime()
            data_manager.insert_usdt(
                user_settings.testing_initial_balance, current_datetime
            )
            user_settings.logger.info("Initializing testing mode")
            user_settings.logger.info(
                f"Inserting USDT: {user_settings.testing_initial_balance}"
            )
        asset_analyzer.run()

    schedule.every(user_settings.execution_frequency_minutes).minutes.at(":00").do(
        asset_analyzer.run
    )
    while True:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()

