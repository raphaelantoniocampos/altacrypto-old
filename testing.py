import random
import string
import time
from core.database_manager import DatabaseManager
from models.crypto_data import CryptoData
from models.snapshot_dict import SnapshotDict
from typing import List
from models.user import User
from models.user_settings import UserSettings
from datetime import datetime, timedelta
from bson.timestamp import Timestamp


def generate_symbol(le=6) -> str:
    random_symbol = "".join(
        random.choices(string.ascii_uppercase + string.digits, k=le)
    )
    return random_symbol


def generate_crypto_data(quantity: int):
    database_manager = DatabaseManager()
    for i in range(quantity):
        symbol = generate_symbol()
        timestamps = []
        for i in range(36):
            timestamp = int((datetime.now() - timedelta(hours=i)).timestamp())
            timestamp = Timestamp(timestamp, 0)
            timestamps.append(timestamp)
        timestamps.sort()
        for timestamp in timestamps:
            crypto_data_list = []
            price = (random.random()) * (random.randint(1, 1000))
            snapshots: List[SnapshotDict] = [
                {"timestamp": timestamp, "price": price}]
            crypto_data = CryptoData(symbol, snapshots)
            crypto_data_list.append(crypto_data)
            database_manager.feed_database(crypto_data_list)


def generate_users(quantity):
    users = []
    for i in range(quantity):
        login = generate_symbol()
        name = f"{login}nome"
        tiers = ["silver" for i in range(9)]
        tiers.append("gold")
        tier = random.choice(tiers)
        api_key = generate_symbol(20)
        secret_key = generate_symbol(20)
        user_settings = UserSettings()
        assets = []
        str_password = generate_symbol()

        user = User(
            login=login,
            name=name,
            tier=tier,
            api_key=api_key,
            secret_key=secret_key,
            user_settings=user_settings,
            assets=assets,
            str_password=str_password,
        )
        users.append(user)
    return users


database_manager = DatabaseManager()
users = generate_users(0)
for user in users:
    database_manager.add_user(user)
users = database_manager.get_all_users({"tier": "silver"})
for user in users:
    print(user)
