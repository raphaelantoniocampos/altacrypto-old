import sys
import time
import logging
import asyncio
import threading
import requests
from datetime import datetime

from flask import Flask, request
from core.database_manager import DatabaseManager
from core.crypto_trader import CryptoTrader
from global_settings import GlobalSettings

app = Flask(__name__)


@app.route("/")
def working():
    return "Bot online!"


@app.route("/shutdown", methods=["POST"])
def shutdown():
    func = request.environ.get("werkzeug.server.shutdown")
    if func is None:
        raise RuntimeError("Not running with the Werkzeug Server")
    func()
    return "Server shutting down..."


def start_flask():
    app.run(host="0.0.0.0", port=5000, debug=True, use_reloader=False)


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
        remaining = (GlobalSettings.EXECUTION_FREQUENCY_MINUTES * 60) - (
            end_time - start_time
        )
        await asyncio.sleep(remaining)
        logger.info(f"Bot running: {datetime.now()}")

if __name__ == "__main__":
    flask_thread = threading.Thread(target=start_flask)
    flask_thread.daemon = True
    flask_thread.start()

    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        print("Exiting due to KeyboardInterrupt")
        try:
            requests.post("http://localhost:5000/shutdown")
        except requests.exceptions.RequestException as e:
            print(f"Error shutting down server: {e}")
        sys.exit()
