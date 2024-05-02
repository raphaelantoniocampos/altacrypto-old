import time
import schedule
import logging

from core.database_manager import DatabaseManager
from core.crypto_trader import CryptoTrader
from utils.global_settings import GlobalSettings


def main():
    logging.basicConfig(level=logging.INFO)
    database_manager = DatabaseManager()

    crypto_trader = CryptoTrader(database_manager)
    schedule.every(GlobalSettings.EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(
        crypto_trader.start
    )

    start_time = time.time()
    crypto_trader.start()
    end_time = time.time()
    elapsed_time = end_time - start_time
    print("Tempo decorrido:", elapsed_time, "segundos")

    while True:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()

