import os
import logging
from dotenv import load_dotenv

load_dotenv()

# Control Constants
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
DB_PATH = "data/trading_info.db"

testing = True
if testing:
    API_KEY = os.environ.get('BINANCE_API_KEY_TEST')
    API_SECRET = os.environ.get('BINANCE_API_SECRET_TEST')

    TESTING_INITIAL_BALANCE = 100

    # if not data_manager.get_database_usdt_balance():
    #    insert_usdt(TESTING_INITIAL_BALANCE)

else:
    API_KEY = os.environ.get('BINANCE_API_KEY')
    API_SECRET = os.environ.get('BINANCE_API_SECRET')

# Usage Constants
INTERVAL_IN_MINUTES = [5, 10, 15, 30, 60]
EXECUTION_FREQUENCY_MINUTES = 5

PERCENTAGE_THRESHOLD = 10
UNDER_PURCHASE_PERCENTAGE = 3
UNDER_HIGHEST_PERCENTAGE = 3
ABOVE_PURCHASE_PERCENTAGE = 200

OPERATION_VALUE_PERCENTAGE = 5
