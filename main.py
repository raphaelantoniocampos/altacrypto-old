import time
import schedule
import logging

from core.crypto_trader import CryptoTrader
from utils.global_settings import GlobalSettings


def main():
    logging.basicConfig(level=logging.INFO)
    global_settings = GlobalSettings()


    crypto_trader = CryptoTrader()
    schedule.every(global_settings.EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(
        crypto_trader.start
    )

    start_time = time.time()  # Registrar o tempo de inicio
    # crypto_trader.start()
    import testing

    end_time = time.time()  # Registrar o tempo de termino

    elapsed_time = end_time - start_time  # Calcular o tempo decorrido
    print("Tempo decorrido:", elapsed_time, "segundos")

    while False:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()
