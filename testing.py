import random
import string
import time
from core.database_manager import DatabaseManager
from models.crypto_data import CryptoData
from models.user import User
from models.user_settings import UserSettings
from datetime import datetime, timedelta


def generate_symbol() -> str:
    random_symbol = "".join(random.choices(string.ascii_uppercase + string.digits, k=6))
    return random_symbol


def generate_documents(quantity: int):
    database_manager = DatabaseManager()
    for i in range(quantity):
        symbol = generate_symbol()
        timestamps = []
        for i in range(36):
            timestamp = int((datetime.now() - timedelta(hours=i)).timestamp())
            timestamps.append(timestamp)
        timestamps.sort()
        for timestamp in timestamps:
            crypto_data_list = []
            price = (random.random()) * (random.randint(1, 1000))
            snapshots = [{"timestamp": timestamp, "price": price}]
            crypto_data = CryptoData(symbol, snapshots)
            crypto_data_list.append(crypto_data)
            database_manager.feed_database(crypto_data_list)


def add_one():
    database_manager = DatabaseManager()
    collection = database_manager.get_collection("crypto_data")
    cursor = collection.find({})
    symbols = []
    for document in cursor:
        symbol = document.get("symbol")
        symbols.append(symbol)

    for symbol in symbols:
        crypto_data = CryptoData(
            symbol=symbol,
            timestamp=(int(time.time())),
            price=((random.random()) * (random.randint(1, 1000))),
        )
        print(crypto_data)


database_manager = DatabaseManager()

user = User(
    login="gomjas",
    name="raphas",
    api_key="132312",
    secret_key="12312",
    user_settings=UserSettings(),
    assets=[],
    str_password="umdois12",
)

# database_manager.add_user(user)
generate_documents(10)

