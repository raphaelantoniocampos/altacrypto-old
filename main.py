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

    start_time = time.time()  # Registrar o tempo de inicio
    crypto_trader.start()
    # import testing

    end_time = time.time()  # Registrar o tempo de termino

    elapsed_time = end_time - start_time  # Calcular o tempo decorrido
    print("Tempo decorrido:", elapsed_time, "segundos")

    while True:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()
