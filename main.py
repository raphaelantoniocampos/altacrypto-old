import time
import schedule
import logging

from core.crypto_trader import CryptoTrader
from utils.global_settings import GlobalSettings
from core.binance_manager import BinanceManager
from data_access.database_manager import DatabaseManager


def main():
    logging.basicConfig(level=logging.INFO)
    global_settings = GlobalSettings()

    binance_manager = BinanceManager()
    database_manager = DatabaseManager()
    crypto_snapshots = binance_manager.fetch_usdt_pairs()
    database_manager.feed_database(crypto_snapshots)

    crypto_trader = CryptoTrader()
    schedule.every(global_settings.EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(
        crypto_trader.run
    )
    crypto_trader.run  # line for testing

    while True:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()
