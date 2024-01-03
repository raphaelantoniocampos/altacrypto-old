import os
import time
from datetime import datetime
import pandas as pd
from dotenv import load_dotenv
from binance.client import Client
import sqlite3
import schedule

load_dotenv()

API_KEY = os.environ.get('BINANCE_API_KEY_TEST')
API_SECRET = os.environ.get('BINANCE_API_SECRET_TEST')

DB_PATH  = "coin_prices.db"

client = Client(API_KEY, API_SECRET, testnet=False)

def fetch_usdt_pairs():
    """
    Retrieves and filters USDT pairs from Binance.
    """
    try:
        tickers = client.get_all_tickers()
        coin_prices = pd.DataFrame(tickers)
        return coin_prices[coin_prices["symbol"].str.endswith("USDT")]
    except Exception as e:
        print(f"Error fetching USDT pairs from Binance: {e}")
        return pd.DataFrame()

def format_symbol(symbol):
    """
    Adds "t" prefix to symbols starting with a digit.
    """
    return 't' + symbol if symbol[0].isdigit() else symbol


def create_table(symbol):
    """
    Creates a table for storing price data of a USD symbol.
    """
    sql = f"""
    CREATE TABLE {symbol} (
        time TEXT PRIMARY KEY,
        date TEXT,
        price REAL
    );
    """
    with sqlite3.connect(DB_PATH) as conn:
        conn.execute(sql)
    
def table_exists(table_name):
    """
    Checks if a table exists in the database.
    """
    sql = f"SELECT name FROM sqlite_master WHERE type='table' AND name='{table_name}'"
    with sqlite3.connect(DB_PATH) as conn:
        return bool(conn.execute(sql).fetchone())

def insert_price(symbol, time_str, date, price):
    """
    Inserts price data for a USD symbol into its table.
    """
    sql = f"INSERT INTO {symbol} (time, date, price) VALUES (?, ?, ?)"
    with sqlite3.connect(DB_PATH) as conn:
        conn.execute(sql, (time_str, date, price))
        
def update_price(symbol, time_str, date, price):
    """
    Updates the price data for a USD symbol.
    """
    sql = f"UPDATE {symbol} SET price = ?, date = ? WHERE time = ?"
    with sqlite3.connect(DB_PATH) as conn:
        conn.execute(sql, (price, date, time_str))


def update_usdt_prices():
    """
    Runs fetch, format, and store operations for all USDT pairs.
    """
    pairs  = fetch_usdt_pairs()
    time_str = datetime.now().strftime('%H:%M')
    date = datetime.now().strftime('%d/%m/%Y')

    for _, row in pairs.iterrows():
        symbol, price = row["symbol"], row["price"]
        formatted_symbol = format_symbol(symbol)

        if not table_exists(formatted_symbol):
            create_table(formatted_symbol)

        with sqlite3.connect(DB_PATH) as conn:
            cursor = conn.cursor()
            cursor.execute(f"SELECT * FROM {formatted_symbol} WHERE time = '{time_str}'")
            row = cursor.fetchone()

        if not row:
            insert_price(formatted_symbol, time_str, date, price)
        else:
            update_price(formatted_symbol, time_str, date, price)

    print(f"USD prices updated at {date} - {time_str}")


def main():
    update_usdt_prices()

if __name__ == "__main__":
    main()
