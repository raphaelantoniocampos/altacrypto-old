import schedule
import time
import csv
import re

import pandas as pd
from datetime import datetime
from constants import *

from models.asset import Asset
from models.transaction_data import TransactionData
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

def timedelta_to_string(delta):
    """Converts a datetime.timedelta object to a human-readable string."""

    seconds = delta.total_seconds()

    days = seconds // 86400  
    seconds %= 86400  

    hours = seconds // 3600  
    seconds %= 3600  

    minutes = seconds // 60  
    seconds %= 60  

    time_string = (
        f"{days} day{'s' if days > 1 else ''} "
        f"{hours} hour{'s' if hours > 1 else ''} "
        f"{minutes} minute{'s' if minutes > 1 else ''} "
        f"{seconds:.0f} second{'s' if seconds > 1 else ''}"
    )

    return time_string.strip()

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
            if asset.symbol != 'USDT':
                asset.update_asset(float(asset_pairs.loc[asset_pairs['symbol'] == asset.symbol, 'price'].iloc[0]))
                data_manager.update_asset(asset)
                if should_asset_be_sold(asset, purchase_recommendations):
                    sell_asset(asset)

    execute_purchase_recommendations(purchase_recommendations)

def should_asset_be_sold(asset, purchase_recommendations):
    recommended_purchase_symbols = set(purchase_recommendations['symbol'])
    if asset.symbol in recommended_purchase_symbols:
        return False
    if asset.variation <= (- UNDER_PURCHASE_PERCENTAGE):
        return True
    if asset.current_price <= asset.highest_price * (1 - UNDER_HIGHEST_PERCENTAGE / 100):
        return True
    if asset.variation >= ABOVE_PURCHASE_PERCENTAGE:
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
    # current_datetime = get_current_datetime()
    assets_dataframe = data_manager.get_assets_dataframe()
    assets_symbols = set(assets_dataframe['symbol'])
    operation_value = get_operation_value()
    for _, row in purchase_recommendations.iterrows():
        symbol = row['symbol']
        if symbol not in assets_symbols:
            if has_balance(operation_value):
                buy_asset(row, operation_value)
                assets_symbols.add(symbol)
    

def has_balance(operation_value):
    balance = data_manager.get_usdt_balance()
    has_balance = balance >= operation_value
    if not has_balance:
        transaction_data = f"""Sem saldo para efetuar transacao
SALDO USDT: {balance}
"""
        log_asset_transaction(message=message)
    return has_balance
    

def update_balance(value):
    balance = data_manager.get_usdt_balance()
    new_balance = round((balance + value), 2)
    data_manager.update_usdt_balance(new_balance)
    return new_balance

def buy_asset(row, operation_value):
    current_datetime = get_current_datetime()
    symbol = row['symbol']
    new_balance = update_balance(-operation_value)
    quantity = operation_value / row['current_price']
    asset = Asset(
        symbol = symbol, 
        quantity = quantity,
        purchase_price = row['current_price'],
        purchase_datetime = current_datetime,
        highest_price = row['current_price'],
        current_price = row['current_price'],
        obs= f"{row['variation']:.2f}% - {row['interval']}"
        )
    time.sleep(3) # Simulates binance transaction
    data_manager.insert_purchase(asset)
    final_balance = data_manager.get_assets_dataframe()['current_value'].sum()
    transaction_data = TransactionData(
        date=current_datetime.date(),
        time=current_datetime.strftime('%H:%M:%S'),
        order_type="Compra",
        quantity=asset.quantity,
        coin=asset.symbol,
        USDT_quantity=operation_value,
        purchase_price=asset.current_price,
        sell_price=None,
        profit_loss=None,
        variation=f'{row["variation"]:.2f}',
        interval=row['interval'],
        trading_fee=0.00,
        USDT_balance=new_balance,
        final_balance=final_balance
    )
    log_asset_transaction(transaction_data)

def sell_asset(asset):
    current_datetime = get_current_datetime()

    time.sleep(3) # Simulates binance transaction

    data_manager.delete_from_assets(asset.symbol)

    profit_loss = asset.calculate_profit_loss()
    new_balance = update_balance(asset.current_value)
    interval = timedelta_to_string(current_datetime - datetime.strptime(asset.purchase_datetime, "%Y-%m-%d %H:%M:%S"))
    final_balance = data_manager.get_assets_dataframe()['current_value'].sum()
    transaction_data = TransactionData(
        date=current_datetime.date(),
        time=current_datetime.strftime('%H:%M:%S'),
        order_type="Venda",
        quantity=asset.quantity,
        coin=asset.symbol,
        USDT_quantity=asset.current_value,
        purchase_price=asset.purchase_price,
        sell_price=asset.current_price,
        profit_loss=profit_loss,
        variation=f'{asset.variation:.2f}',
        interval=interval,
        trading_fee=0.00,
        USDT_balance=new_balance,
        final_balance=final_balance
    )
    log_asset_transaction(transaction_data)

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

def log_asset_transaction(transaction_data):
   current_datetime = get_current_datetime() 
   current_date = current_datetime.date()
   file_name = f'logs/log-{current_date}.csv'

   fieldnames = ["Data", "Hora", "Tipo de ordem", "Quantidade", "Moeda", "Quantidade USDT", "Preco de compra", "Preco de venda", "Lucro/prejuizo", "Variacao", "Intervalo", "Taxa de negociacao", "Saldo USDT", "Saldo final"]
   try:
       with open(file_name, 'r') as file:
           csv.reader(file)  
   except FileNotFoundError:
        with open(file_name, 'w', newline='') as file:
            writer = csv.DictWriter(file, fieldnames=fieldnames)
            writer.writeheader()

   with open(file_name, 'a', newline='') as file:
        writer = csv.DictWriter(file, fieldnames=fieldnames)
        row = {
            "Data": transaction_data.date,
            "Hora": transaction_data.time,
            "Tipo de ordem": transaction_data.order_type,
            "Quantidade": transaction_data.quantity,
            "Moeda": transaction_data.coin,
            "Quantidade USDT": transaction_data.USDT_quantity,
            "Preco de compra": transaction_data.purchase_price,
            "Preco de venda": transaction_data.sell_price,
            "Lucro/prejuizo": transaction_data.profit_loss,
            "Variacao": transaction_data.variation,
            "Intervalo": transaction_data.interval,
            "Taxa de negociacao": transaction_data.trading_fee,
            "Saldo USDT": transaction_data.USDT_balance,
            "Saldo final": transaction_data.final_balance
        }
        writer.writerow(row)
        print(transaction_data)

def insert_usdt(value):
    current_datetime = get_current_datetime()
    data_manager.insert_usdt(value, current_datetime)

def get_operation_value():
    balance = data_manager.get_usdt_balance()
    operation_value =  round(balance / (100 / OPERATION_VALUE_PERCENTAGE), 2)
    if operation_value < 10:
        operation_value = 10
    return operation_value

if __name__ == "__main__":
    if not data_manager.get_usdt_balance():
        insert_usdt(TESTING_INITIAL_BALANCE)

    fetch_and_analyze_assets_data()

    schedule.every(EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(fetch_and_analyze_assets_data)
    while True:
        schedule.run_pending()
        time.sleep(1)
