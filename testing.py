import random
import string
import time
from core.database_manager import DatabaseManager
from core.binance_manager import BinanceManager
from models.user_settings import UserSettings
from typing import List
from models.user import User
from models.asset import Asset
from models.order import Order
from datetime import datetime, timedelta, timezone
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


def generate_users(quantity, usd_balance):
    users = []
    names = [
        "Alice",
        "Bob",
        "Charlie",
        "David",
        "Emma",
        "Frank",
        "Grace",
        "Henry",
        "Isabella",
        "Jack",
        "Kate",
        "Liam",
        "Mia",
        "Noah",
        "Olivia",
        "Peter",
        "Quinn",
        "Rachel",
        "Samuel",
        "Tiffany",
        "Uma",
        "Victor",
        "Wendy",
        "Xavier",
        "Yasmine",
        "Zachary",
        "Abigail",
        "Benjamin",
        "Chloe",
        "Daniel",
        "Emily",
        "Fiona",
        "George",
        "Hannah",
        "Ian",
        "Jessica",
        "Kevin",
        "Lily",
        "Matthew",
        "Natalie",
        "Oscar",
        "Penelope",
        "Quentin",
        "Rebecca",
        "Sarah",
        "Thomas",
        "Ursula",
        "Vincent",
        "Wendell",
        "Amanda",
        "Bruno",
        "Carla",
        "Diego",
        "Érica",
        "Fábio",
        "Gabriela",
        "Hugo",
        "Isadora",
        "João",
        "Kátia",
        "Lucas",
        "Mariana",
        "Natália",
        "Otávio",
        "Paula",
        "Rafael",
        "Sara",
        "Thiago",
        "Úrsula",
        "Vitória",
        "Wagner",
        "Ximena",
        "Yasmin",
        "Zeca",
        "Adriana",
        "Bernardo",
        "Camila",
        "Davi",
        "Elaine",
        "Fernando",
        "Gisele",
        "Hélio",
        "Íris",
        "Júlio",
        "Karen",
        "Leandro",
        "Márcia",
        "Nathan",
        "Olívia",
        "Patrícia",
        "Rodrigo",
        "Sílvia",
        "Túlio",
        "Valentina",
        "William",
        "Xuxa",
        "Yago",
        "Zuleide",
    ]
    for i in range(quantity):
        login = generate_symbol()
        name = random.choice(names)
        api_key = generate_symbol(20)
        secret_key = generate_symbol(20)
        user_settings = UserSettings()
        str_password = generate_symbol()

        user = User(
            login=login,
            name=name,
            api_key=api_key,
            secret_key=secret_key,
            user_settings=user_settings,
            usd_balance=usd_balance,
            str_password=str_password,
        )
        users.append(user)
    return users


def add_users(quantity, usd_balance):
    database_manager = DatabaseManager()
    users = generate_users(quantity, usd_balance)
    for user in users:
        database_manager.add_user(user)


def generate_assets(crypto_snapshots, users, quantity):
    assets = []
    for i in range(quantity):
        snap = crypto_snapshots[random.randint(0, len(crypto_snapshots))]
        symbol = snap.symbol
        quantity = round(random.uniform(0, 100), 2)
        if quantity < 0:
            quantity *= -1
        purchase_price = snap.price
        purchase_datetime = snap.datetime
        highest_price = snap.price
        current_price = snap.price
        user_id = users[random.randint(0, (len(users) - 1))].id
        asset = Asset(
            user_id,
            symbol,
            quantity,
            purchase_price,
            purchase_datetime,
            highest_price,
            current_price,
        )
        assets.append(asset)
    return assets


def add_assets(maximum: int = 10):
    database_manager = DatabaseManager()
    binance_manager = BinanceManager()
    crypto_snapshots = binance_manager.fetch_usdt_pairs()
    users = database_manager.get_all_users()
    quantity = random.randint(0, maximum)
    assets = generate_assets(crypto_snapshots, users, quantity)
    for asset in assets:
        database_manager.add_asset(asset)


# add_users(20, 100)
# rn = random.randint(0, 20)
# add_users(rn, 100)
# print(f"({20 + rn}) users 100 criados")
#
# add_users(10, 500)
# rn = random.randint(0, 10)
# add_users(rn, 500)
# print(f"({10 + rn}) users 500 criados")
#
# add_users(5, 1000)
# rn = random.randint(0, 5)
# add_users(rn, 1000)
# print(f"({5 + rn}) users 1000 criados")
# # add_assets(100)
