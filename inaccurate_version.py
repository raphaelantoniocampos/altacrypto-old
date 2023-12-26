import os
import time
import datetime
import statistics

import pandas as pd
from dotenv import load_dotenv
from binance.client import Client

import pprint
load_dotenv()

INTERVALS = [1, 5, 15, 30, 60, 120, 240, 480, 720]


api_key = os.environ.get('BINANCE_API_KEY_TEST')
api_secret = os.environ.get('BINANCE_API_SECRET_TEST')

client = Client(api_key, api_secret, testnet=False)

def format_seconds(seconds):
    """
    Formats a number of seconds into a human-readable string.

    Args:
        seconds: The number of seconds.

    Returns:
        A human-readable string of the number of seconds.
    """
    if seconds < 0:
        seconds *= -1
    hours = int(seconds // 3600)
    minutes = int((seconds % 3600) // 60)
    milliseconds = int((seconds % 1) * 1000)
    seconds = int(seconds % 60)

    formated_time = ""
    hours_string = f"{hours:02d}h:"
    minutes_string = f"{minutes:02d}min:"
    seconds_string = f"{seconds:02d}s:"
    milliseconds_string = f"{milliseconds:03d}ms"
    formated_time = f"{hours_string if (hours != 0) else ''}{minutes_string if (minutes != 0) else ''}{seconds_string}{milliseconds_string}"
    return formated_time

def format_time(seconds):
    return datetime.datetime.fromtimestamp(int(seconds)/1000)


def fetch_kline_variations(symbol):
    print(f"Symbol {symbol}")
    klines = list(client.get_klines(symbol=symbol, interval='1m', limit=720))
    closing_price = float(klines[-1][4])
    variations = []
    # print(f"closing price {closing_price}")
    for interval in INTERVALS:
        opening_price = float(klines[-(interval)][1])
        variation = ((closing_price - opening_price) / opening_price) * 100
        variations.append(variation)
        print(f"{interval}m: {variation:.2f}%")
    print("\n")
    return variations


    

def main():
    start_time = time.perf_counter()

    tickers = client.get_all_tickers()
    df = pd.DataFrame(tickers)
    usdt_pairs  = df[df['symbol'].str.endswith("USDT")].head(1)
    print(f"len(usdt_pairs): {len(usdt_pairs)}")

    for _, row in usdt_pairs.iterrows():
        symbol = row['symbol']
        fetch_kline_variations(symbol)


    #print test
    # print(f'Média das variações\n')
        
    end_time = time.perf_counter()
    runtime = end_time - start_time
    print(f"Tempo de execução: {format_seconds(runtime)}")
    
if __name__ == "__main__":
    main()



