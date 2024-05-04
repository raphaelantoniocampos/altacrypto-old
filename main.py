import time
import logging
import asyncio

from core.database_manager import DatabaseManager
from core.crypto_trader import CryptoTrader
from utils.global_settings import GlobalSettings


async def main():
    logging.basicConfig(level=logging.INFO)
    database_manager = DatabaseManager()

    crypto_trader = CryptoTrader(database_manager)
    while True:
        start_time = time.time()
        await crypto_trader.start()
        end_time = time.time()
        remaining = (GlobalSettings.EXECUTION_FREQUENCY_MINUTES * 60) - (
            end_time - start_time
        )
        print(f"Tempo decorrido: {end_time - start_time}")
        await asyncio.sleep(remaining)

if __name__ == "__main__":
    asyncio.run(main())
