import sys
import time
import logging
import asyncio
import threading

from flask import Flask
from core.database_manager import DatabaseManager
from core.crypto_trader import CryptoTrader
from global_settings import GlobalSettings

app = Flask(__name__)


def start_flask():
    app.run(host="172.31.16.0/20", port=5000, debug=True)


async def main():
    """
    The main entry point of the application.
    """
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
        await asyncio.sleep(remaining)

if __name__ == "__main__":
    flask_thread = threading.Thread(target=start_flask())
    flask_thread.daemon = True
    flask_thread.start()

    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        print("Exiting due to KeyboardInterrupt")
        sys.exit()
