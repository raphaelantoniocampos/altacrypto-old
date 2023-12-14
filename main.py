import os
import time
import datetime

import pandas as pd
from dotenv import load_dotenv
from binance.client import Client
load_dotenv()

import pprint as pprint

api_key = os.environ.get('BINANCE_API_KEY_TEST')
api_secret = os.environ.get('BINANCE_API_SECRET_TEST')

client = Client(api_key, api_secret, testnet=False)


def update_variation():
    while True:
        tickers = client.get_all_tickers()

        df = pd.DataFrame(tickers)
        current_pairs = df[df['symbol'].str.endswith("USDT")]

        time.sleep(30)
        previous_pairs = current_pairs

        tickers = client.get_all_tickers()
        df = pd.DataFrame(tickers)
        current_pairs = df[df['symbol'].str.endswith("USDT")]

        for index, row in current_pairs.iterrows():
            previous_pair = previous_pairs.loc[index]
            if previous_pair['symbol'] == row['symbol']:
                previous_price = float(previous_pair['price'])
                current_price = float(row['price'])
                variation = ((current_price - previous_price) / previous_price) * 100
                print(f"pair {row['symbol']} - variation {variation}")


def velas():

    tickers = client.get_all_tickers()

    df = pd.DataFrame(tickers)
    current_pairs = df[df['symbol'].str.endswith("USDT")]

    for _, row in current_pairs.iterrows():
        candles = client.get_klines(symbol=row['symbol'], interval=Client.KLINE_INTERVAL_1MINUTE, limit=10)
        print(f"moeda: {row['symbol']} - {candles}")
        print("\n")

def test_ontem():
    tickers = client.get_all_tickers()

    df = pd.DataFrame(tickers)
    current_pairs = df[df['symbol'].str.endswith("USDT")]

    balance = client.get_asset_balance(asset='USDT')
    pprint.pprint(balance)

    tickers = client.get_all_tickers()

    df = pd.DataFrame(tickers)
    pares = df[df['symbol'].str.endswith("USDT")]
    price = 0
    for _, pair in pares.iterrows():
        if pair['symbol'] == 'BTCUSDT':
            price = pair['price']



def convert_time(time):
    your_dt = datetime.datetime.fromtimestamp(time / 1000)
    return your_dt.strftime("%Y-%m-%d -- %H:%M:%S")


while True:
    intervals = ['1m', '5m', '15m', '30m', '1h', '2h', '4h', '8h', '12h', '1d']

    for interval in intervals:
        klines = client.get_klines(symbol='BTCUSDT', interval=interval, limit=1)
        kline = klines[0]

        old_price = float(kline[1])
        new_price = float(kline[4])
        open_time = kline[0]
        close_time = kline[6]

        variation = ((new_price - old_price) / old_price) * 100
        print(f"BTCUSDT\nOpen Time: {convert_time(open_time)}\nClose time: {convert_time(close_time)}\nIntervalo: {interval}\nVariação: {variation}%\n")
    
    time.sleep(60)