import random
import string
import time
from core.database_manager import DatabaseManager
from models.user_settings import UserSettings
from typing import List
from models.user import User
from datetime import datetime, timedelta
from bson.timestamp import Timestamp


def generate_symbol(le=6) -> str:
    random_symbol = "".join(
        random.choices(string.ascii_uppercase + string.digits, k=le)
    )
    return random_symbol


def create_ttl_index():
    """Create TTL index on timestamp field."""
    database_manager = DatabaseManager()
    collection = database_manager.get_collection("crypto_snapshots")
    # Create TTL index on timestamp field, set expiration to 24 hours
    collection.create_index("timestamp", expireAfterSeconds=6)


def generate_crypto_snapshots(quantity: int):
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
            crypto_snapshots = []
            price = (random.random()) * (random.randint(1, 1000))
            snapshots: List[SnapshotDict] = [
                {"timestamp": timestamp, "price": price}]
            crypto_snapshot = CryptoSnapshot(symbol, snapshots)
            crypto_snapshots.append(crypto_snapshot)
            database_manager.feed_database(crypto_snapshots)


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
users = generate_users(5)
for user in users:
    database_manager.add_user(user)
