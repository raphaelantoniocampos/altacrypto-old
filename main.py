import os
import time
from datetime import datetime, timedelta
import pandas as pd
from dotenv import load_dotenv
from binance.client import Client
import sqlite3

load_dotenv()

api_key = os.environ.get('BINANCE_API_KEY_TEST')
api_secret = os.environ.get('BINANCE_API_SECRET_TEST')
db_path = "coin_prices.db"

client = Client(api_key, api_secret, testnet=False)

def get_all_prices():
    """Retrieves all price tickers from Binance."""
    tickers = client.get_all_tickers()
    df = pd.DataFrame(tickers)
    usdt_pairs = df[df['symbol'].str.endswith("USDT")].reset_index(drop=True)
    return usdt_pairs

def main():
    while True:
        print(get_all_prices())
        time.sleep(30)  # Wait for 1 minute before fetching again

if __name__ == "__main__":
    main()
