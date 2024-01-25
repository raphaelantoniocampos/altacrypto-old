import os
from dotenv import load_dotenv

load_dotenv()

API_KEY = os.environ.get('BINANCE_API_KEY_TEST')
API_SECRET = os.environ.get('BINANCE_API_SECRET_TEST')
DB_PATH  = "trading_info.db"
INTERVAL_IN_MINUTES = [5, 10, 15, 30, 60]
PERCENTAGE_THRESHOLD = 5