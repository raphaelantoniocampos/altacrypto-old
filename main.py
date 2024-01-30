import schedule
import time

import pandas as pd
from datetime import datetime
from constants import *

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

def fetch_and_analyze_assets_data():
    """Fetches price data, updates the database, analyzes assets, and generates recommendations."""

    asset_pairs = binance_api.fetch_usdt_pairs()
    current_timestamp = get_current_timestamp()
    
    database_feeder.update_database(asset_pairs, current_timestamp)
    evaluate_assets(asset_pairs)
                   
def evaluate_assets(asset_pairs):
    """Analyzes existing assets and makes sell decisions if necessary."""
    purchase_recommendations = identify_purchase_recommendations(asset_pairs)
    assets_dataframe = data_manager.get_assets_dataframe()

    if not assets_dataframe.empty:
        for _, row in assets_dataframe.iterrows():
            asset = Asset.from_series(row)
            asset = update_asset(asset, asset_pairs)
            if should_asset_be_sold(asset, purchase_recommendations):
                sell_asset(asset)

    execute_purchase_recommendations(purchase_recommendations)
    

def update_asset(asset, asset_pairs):
    asset.current_price = float(asset_pairs.loc[asset_pairs['symbol'] == asset.symbol, 'price'].iloc[0])
    if asset.current_price > asset.highest_price:
        asset.highest_price = asset.current_price
    asset.variation = calculate_asset_variation(asset)
    data_manager.update_asset(asset)
    return asset

def should_asset_be_sold(asset, purchase_recommendations):
    recommended_purchase_symbols = set(purchase_recommendations['symbol'])
    if asset.symbol in recommended_purchase_symbols:
        return False
    if asset.variation <= (- UNDER_PURCHASE_PERCENTAGE):
        return True
    if asset.current_price <= asset.highest_price * (1 - UNDER_HIGHEST_PERCENTAGE / 100):
        return True
    if asset.current_price >= asset.highest_price * (1 + ABOVE_PURCHASE_PERCENTAGE / 100):
        return True
    return False

def calculate_asset_variation(asset):
    return round((((asset.current_price - asset.purchase_price) / asset.current_price) * 100), 2)

def identify_purchase_recommendations(asset_pairs):
    current_datetime = get_current_datetime()
    purchase_recommendations = pd.DataFrame()
    for interval_in_minutes in INTERVAL_IN_MINUTES:
        interval_index = int(interval_in_minutes / EXECUTION_FREQUENCY_MINUTES)
        interval_dataframe = generate_price_change_data(
            interval_index, asset_pairs, current_datetime
            )
        if not interval_dataframe.empty:
            interval_recommendations = process_interval_data(interval_dataframe)
            purchase_recommendations = pd.concat([purchase_recommendations, interval_recommendations])
    return purchase_recommendations

def process_interval_data(interval_dataframe):
    mean_variation = interval_dataframe['variation'].mean()
    interval_recommendations = interval_dataframe[
        interval_dataframe['variation'] >= mean_variation + PERCENTAGE_THRESHOLD
        ]
    return interval_recommendations
        
def execute_purchase_recommendations(purchase_recommendations):
    current_datetime = get_current_datetime()
    assets_dataframe = data_manager.get_assets_dataframe()
    assets_symbols = set(assets_dataframe['symbol'])
    for _, row in purchase_recommendations.iterrows():
        symbol = row['symbol']
        if symbol not in assets_symbols:
            asset = Asset(symbol, current_datetime, row['current_price'], row['current_price'], row['current_price'], row['variation'], obs=row['interval'])
            assets_symbols.add(asset.symbol)
            buy_asset(asset)

def buy_asset(asset):
    data_manager.insert_purchase(asset)
    message =f"""{asset.symbol} COMPRADO - {asset.purchase_datetime}
VARIACAO: {asset.variation:.2f}% 
INTERVALO: {asset.obs}
PRECO DE COMPRA: {asset.current_price}
_____________________________________
"""
    log_asset_transaction(message)

def sell_asset(asset):
    current_datetime = get_current_datetime()
    data_manager.delete_from_assets(asset.symbol)
    message = f"""{asset.symbol} VENDIDO - {current_datetime}
ADQUIRIDO EM {asset.purchase_datetime}
PRECO DE COMPRA: {asset.purchase_price}
MAIOR PRECO: {asset.highest_price}
PRECO DE VENDA: {asset.current_price}
VARIACAO: {asset.variation:.2f}%
_____________________________________
"""
    log_asset_transaction(message)

def generate_price_change_data(interval_index, usdt_pairs, current_datetime):
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

def log_asset_transaction(message):
    current_datetime = get_current_datetime().date()
    with open(f'logs/log-{current_datetime}.txt', 'a') as file:
        file.write(f'{message}\n')
    print(message)

if __name__ == "__main__":
    fetch_and_analyze_assets_data()

    schedule.every(EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(fetch_and_analyze_assets_data)
    while True:
        schedule.run_pending()
        time.sleep(1)
