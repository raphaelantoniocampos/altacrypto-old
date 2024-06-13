import sys
import time
import logging
import asyncio
import threading
import requests
from datetime import datetime

from core.database_manager import DatabaseManager
from core.crypto_trader import CryptoTrader
from global_settings import GlobalSettings

async def main():
    """
    The main entry point of the application.
    """
    logging.basicConfig(level=logging.INFO)
    logger = logging.getLogger(__name__)
    database_manager = DatabaseManager()

    crypto_trader = CryptoTrader(database_manager)
    logger.info(f"Bot started: {datetime.now()}")
    while True:
        start_time = time.perf_counter()
        await crypto_trader.start()
        end_time = time.perf_counter()
        logger.info(f"{__name__} took: {end_time - start_time}ms")
        remaining = (GlobalSettings.EXECUTION_FREQUENCY_MINUTES * 60) - (
            end_time - start_time
        )
        await asyncio.sleep(remaining)
        logger.info(f"Bot running: {datetime.now()}")

if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        print("Exiting due to KeyboardInterrupt")
        sys.exit()
