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

api_key = os.environ.get('BINANCE_API_KEY_TEST')
api_secret = os.environ.get('BINANCE_API_SECRET_TEST')

client = Client(api_key, api_secret, testnet=False)

def fetch_kline_variations(symbol, interval):
    dicts = {}
    klines = client.get_klines(symbol=symbol, interval=interval, limit=1)
    kline = klines[0]

    old_price = float(kline[1])
    new_price = float(kline[4])

    variation = ((new_price - old_price) / old_price) * 100

    dicts[symbol] = variation
    return dicts

def main():
    start_time = time.perf_counter()

    tickers = client.get_all_tickers()
    df = pd.DataFrame(tickers)
    usdt_pairs  = df[df['symbol'].str.endswith("USDT")].head()

    dictionaries = {interval: [] for interval in INTERVALS}

    for _, row in usdt_pairs.iterrows():
        symbol = row['symbol']

        for interval in INTERVALS:
            dictionaries[interval].append(fetch_kline_variations(symbol, interval))


    #print test
    print(f'Média das variações\n')
    for interval in INTERVALS:
        values = [value for sub_dict in dictionaries[interval] for value in sub_dict.values()]
        mean = statistics.mean(values)
        print(f"Média {interval}: {mean:.2f}%")
        
    end_time = time.perf_counter()
    runtime = end_time - start_time
    print(f"Tempo de execução: {runtime:.2f} segundos")
    
if __name__ == "__main__":
    main()



