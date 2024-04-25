import random
import string
import time
from core.database_manager import DatabaseManager
from models.crypto_snapshot import CryptoSnapshot
from datetime import datetime, timedelta
import pandas as pd


def generate_symbol() -> str:
    random_symbol = "".join(random.choices(string.ascii_uppercase + string.digits, k=6))
    return random_symbol


def generate_documents(quantity: int):
    for i in range(quantity):
        symbol = generate_symbol()
        timestamps = []
        for i in range(36):
            timestamp = int((datetime.now() - timedelta(hours=i)).timestamp())
            timestamps.append(timestamp)
        timestamps.sort()
        for timestamp in timestamps:
            price = (random.random()) * (random.randint(1, 1000))
            crypto_snapshot = CryptoSnapshot(symbol, timestamp, price)
            database_manager = DatabaseManager()
            database_manager.add_crypto_snapshot(crypto_snapshot)


# generate_documents(10)


def add_one():
    database_manager = DatabaseManager()
    collection = database_manager.get_collection("crypto_snapshots_tests")
    cursor = collection.find({})
    symbols = []
    for document in cursor:
        symbol = document.get("symbol")
        symbols.append(symbol)

    for symbol in symbols:
        crypto_snapshot = CryptoSnapshot(
            symbol=symbol,
            timestamp=(int(time.time())),
            price=((random.random()) * (random.randint(1, 1000))),
        )
        print(crypto_snapshot)
        database_manager.add_crypto_snapshot(crypto_snapshot)


"""
database_manager = DatabaseManager()
crypto_snapshot = CryptoSnapshot(symbol="XWBJ8O", timestamp=100000, price=2000.00)
timestamps = []
for i in range(36):
    timestamp = int((datetime.now() - timedelta(hours=i)).timestamp())
    timestamps.append(timestamp)

"""
database_manager = DatabaseManager()
print(database_manager.get_all_crypto_snapshots())
