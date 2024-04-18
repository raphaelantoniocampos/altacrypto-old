import time
import logging
import schedule

from core.asset_analyzer import AssetAnalyzer
from core.balance_manager import BalanceManager
from core.binance_manager import BinanceManager
from core.data_manager import DataManager
from utils.global_settings import GlobalSettings


def main():
    logging.basicConfig(level=logging.INFO)
    logger = logging.getLogger("altacrypto")

    logger.info("Starting bot")
    global_settings = GlobalSettings()
    binance_manager = BinanceManager()
    data_manager = DataManager()
    balance_manager = BalanceManager(data_manager)
    asset_analyzer = AssetAnalyzer(
        binance_manager, data_manager, balance_manager)
    schedule.every(global_settings.execution_frequency_minutes).minutes.at(":00").do(
        asset_analyzer.run
    )

    while True:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()
