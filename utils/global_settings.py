import logging


class GlobalSettings:
    """TODO: Document Class"""

    INTERVAL_IN_MINUTES = [5, 10, 15, 30, 60]
    EXECUTION_FREQUENCY_MINUTES = 5
    CRYPTO_PRICES_DB_PATH = "data\\crypto_prices.db"
    USER_DATA_DB_PATH = "data\\user_data.db"

    logging.basicConfig(level=logging.INFO)
    logger = logging.getLogger("altacrypto")

