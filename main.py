import schedule
import time

import pandas as pd
from datetime import datetime
from constants import *

from price_snapshot import PriceSnapshot
from asset import Asset
from database_feeder import DatabaseFeeder
from binance_api import BinanceAPI
from data_manager import DataManager



data_manager = DataManager(DB_PATH)
database_feeder = DatabaseFeeder(data_manager)
binance_api = BinanceAPI(API_KEY, API_SECRET)

def get_current_datetime(timestamp = None):
    if not timestamp:
        timestamp = get_current_timestamp()
    return datetime.fromtimestamp(timestamp)

def get_current_timestamp():
    return int(time.time())

def fetch_and_analyze_price_data():
    """Fetches price data, updates the database, analyzes assets, and generates recommendations."""

    usdt_pairs = binance_api.fetch_usdt_pairs()
    timestamp = get_current_timestamp()
    current_datetime = get_current_datetime(timestamp)

    database_feeder.update_database(usdt_pairs, timestamp)
    analyze_assets(usdt_pairs)
    analyze_price_variations(usdt_pairs, current_datetime)
                   
def analyze_assets(usdt_pairs):
    """Analyzes existing assets and makes sell decisions if necessary."""
    assets_dataframe = data_manager.get_assets_dataframe()
    if not assets_dataframe.empty:
        for _, row in assets_dataframe.iterrows():
            asset = Asset.from_series(row)
            asset.current_price = float(usdt_pairs.loc[usdt_pairs['symbol'] == asset.symbol, 'price'].iloc[0])
            update_asset(asset)
            if should_sell_asset(asset):
                sell_asset(asset)

def update_asset(asset):
    if asset.current_price > asset.highest_price:
        asset.highest_price = asset.current_price
    variation = calculate_asset_variation(asset)
    data_manager.update_asset(asset, variation)

def should_sell_asset(asset):
    return asset.current_price <= (asset.highest_price * 0.95)

def calculate_asset_variation(asset):
    return round((((asset.current_price - asset.purchase_price) / asset.current_price) * 100), 2)

def analyze_price_variations(usdt_pairs, current_datetime):
    for interval_in_minutes in INTERVAL_IN_MINUTES:
        interval_index = int(interval_in_minutes / 5)
        interval_dataframe = create_variation_dataframe(
            interval_index, usdt_pairs, current_datetime
            )
        if not interval_dataframe.empty:
            process_interval_dataframe(interval_dataframe)

def process_interval_dataframe(interval_dataframe):
    mean_variation = interval_dataframe['variation'].mean()
    purchase_recommendations = interval_dataframe[
        interval_dataframe['variation'] >= mean_variation + PERCENTAGE_THRESHOLD
        ]

    if not purchase_recommendations.empty:
        analyze_purchase_recommendations(purchase_recommendations)
        
def analyze_purchase_recommendations(purchase_recommendations):
    current_datetime = get_current_datetime()
    assets_dataframe = data_manager.get_assets_dataframe()
    assets_symbols = set(assets_dataframe['symbol'])

    for _, row in purchase_recommendations.iterrows():
        symbol = row['symbol']
        if symbol not in assets_symbols:
            asset = Asset(symbol, current_datetime, row['current_price'], row['current_price'], row['current_price'], 0)
            buy_asset(asset)

def buy_asset(asset):
    data_manager.insert_purchase(asset)
    message = f"""{asset.symbol} COMPRADO POR {asset.current_price}\n{asset.purchase_datetime}"""
    log_operation(message)

def sell_asset(asset):
    current_datetime = get_current_datetime()
    data_manager.delete_from_assets(asset.symbol)
    message = f"""
{asset.symbol} VENDIDO POR {asset.current_price}
SALDO: {asset.current_price - asset.purchase_price} - VARIACAO: {asset.variation}
{current_datetime}
"""
    log_operation(message)

def create_variation_dataframe(interval_index, usdt_pairs, current_datetime):
    """Creates a DataFrame containing price variations for the given interval."""
    variation_data = []
    interval_time = None

    for df in data_manager.get_all_coins_dataframes(usdt_pairs):
        if len(df) > interval_index:
            last_entry = df.iloc[0]
            past_entry = df.iloc[interval_index]
            current_price = last_entry['price']
            past_price = past_entry['price']
            interval_time = pd.to_datetime((last_entry['timestamp'] - past_entry['timestamp']), unit='s').strftime("%H:%M:%S")            
            variation_percent = ((current_price - past_price) / current_price) * 100
            variation_data.append({
                'symbol': last_entry['symbol'],
                'interval': interval_time,
                'current_datetime': current_datetime,
                'current_price': current_price,
                'past_price': past_price,
                'variation': variation_percent
            })
    return pd.DataFrame(variation_data)

def log_operation(message):
    with open('log.txt', 'a') as file:
        file.write(f'{message}\n')
    print(message)

if __name__ == "__main__":
    fetch_and_analyze_price_data()

    schedule.every(5).minutes.at(":00").do(fetch_and_analyze_price_data)
    while True:
        schedule.run_pending()
        time.sleep(1)
