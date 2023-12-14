import os
import time
import datetime
import statistics

import pandas as pd
from dotenv import load_dotenv
from binance.client import Client

import pprint
load_dotenv()

INTERVALS = ['1m', '5m', '15m', '30m', '1h', '2h', '4h', '8h', '12h', '1d']
INTERVALS = ['1m']

api_key = os.environ.get('BINANCE_API_KEY_TEST')
api_secret = os.environ.get('BINANCE_API_SECRET_TEST')

client = Client(api_key, api_secret, testnet=False)

def format_seconds(seconds):
    if seconds >= 60:
        minutes = seconds // 60
        seconds = seconds - minutes * 60
        return f"{minutes:.0f}m:{seconds:.2f}s"
    else:
        return f"{seconds:.2f}s"
    
def fetch_kline_variations(symbol, interval):
    klines = client.get_klines(symbol=symbol, interval=interval, limit=1)
    kline = klines[0]
    old_price, new_price = float(kline[1]), float(kline[4])
    variation = ((new_price - old_price) / old_price) * 100
    return variation

def main():
    start_time = time.perf_counter()

    tickers = client.get_all_tickers()
    df = pd.DataFrame(tickers)
    usdt_pairs  = df[df['symbol'].str.endswith("USDT")].head()

    variations = {}

    for _, row in usdt_pairs.iterrows():
        symbol = row['symbol']

        for interval in INTERVALS:
            variation = fetch_kline_variations(symbol, interval)
            if interval not in variations:
                variations[interval] = []
            variations[interval].append(variation)


    #print test
    print(f'Média das variações\n')
    for interval in INTERVALS:
        mean = statistics.mean(variations[interval])
        print(f"Média {interval}: {mean:.2f}%")
        
    end_time = time.perf_counter()
    runtime = end_time - start_time
    print(f"Tempo de execução: {format_seconds(runtime)}")
    
if __name__ == "__main__":
    main()



