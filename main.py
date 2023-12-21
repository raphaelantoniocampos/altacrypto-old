import os
import time
from datetime import datetime, timedelta
import pandas as pd
from dotenv import load_dotenv
from binance.client import Client

load_dotenv()

api_key = os.environ.get('BINANCE_API_KEY_TEST')
api_secret = os.environ.get('BINANCE_API_SECRET_TEST')

client = Client(api_key, api_secret, testnet=False)


def get_all_prices():
    tickers = client.get_all_tickers()
    df = pd.DataFrame(tickers)
    usdt_pairs = df[df['symbol'].str.endswith("USDT")].reset_index(drop=True)
    return usdt_pairs

def main():
    start_time = datetime.now()
    while True:
        now = datetime.now()
        interval = now - start_time

        if interval >= timedelta(seconds=60):
            prices = get_all_prices()
            print(prices)
            start_time = datetime.now()
            time.sleep(60)  # Aguardar 1 minuto antes de verificar novamente

if __name__ == "__main__":
    main()