import pandas as pd
import os
from dotenv import load_dotenv
import pymongo
import logging
import bson
from typing import List

from datetime import datetime
from models.crypto_snapshot import CryptoSnapshot
from models.user import User
from models.asset import Asset
from models.user_settings import UserSettings


class DatabaseManager:
    """TODO: Document class"""

    def __init__(self):
        """TODO: Document method"""
        self.dbname = self.get_database()
        self.logger = logging.getLogger(__name__)

    def get_database(self) -> pymongo.MongoClient:
        """TODO: Document method"""
        load_dotenv()
        mongo_user = os.getenv("MONGO_USER")
        mongo_password = os.getenv("MONGO_PASSWORD")
        connection_string = f"mongodb+srv://{mongo_user}:{mongo_password}@cluster0.wovexfa.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0"
        client = pymongo.MongoClient(connection_string)
        return client["altadata"]

    def get_collection(self, collection_name: str) -> pymongo.collection.Collection:
        """TODO: Document method"""
        return self.dbname[f"{collection_name}"]

    # CryptoSnapshots
    def feed_database(self, crypto_snapshots: List[CryptoSnapshot]) -> None:
        """
        Updates the database with crypto snapshots.

        Args:
        crypto_snapshots (List['CryptoSnapshot']): containing asset pairs and their prices.
        """
        try:
            collection = self.get_collection("crypto_snapshots")
            self.create_ttl_index(
                collection_name="crypto_snapshots",
                index_name="datetime",
                ascending=False,
                expire_after_seconds=86400,
            )
            bulk_operations = []
            for crypto_snapshot in crypto_snapshots:
                operation = self._create_add_crypto_snapshot_operation(crypto_snapshot)
                bulk_operations.append(operation)

            if bulk_operations:
                collection.bulk_write(bulk_operations)

            current_datetime = datetime.now()
            self.logger.info(f"Crypto Snapshots updated at {current_datetime}")

        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error adding feeding database: {e}")

    def _create_add_crypto_snapshot_operation(
        self, crypto_snapshot: CryptoSnapshot
    ) -> pymongo.operations.UpdateOne | pymongo.operations.InsertOne:
        """TODO: Document method"""
        operation = pymongo.InsertOne(
            {
                "symbol": crypto_snapshot.symbol,
                "datetime": crypto_snapshot.datetime,
                "price": crypto_snapshot.price,
            },
        )

        return operation

    def get_all_crypto_snapshots(self) -> List[pd.DataFrame]:
        """
        Retrieves price data for all USD symbols as a list of DataFrames.

        Returns:
            list: List of DataFrames containing crypto data for each symbol.
        """
        try:
            collection = self.get_collection("crypto_snapshots")
            cursor = collection.find()
            crypto_snapshots = []
            for document in cursor:
                crypto_snapshots.append(CryptoSnapshot.from_dict(document))
            return crypto_snapshots
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error getting crypto snapshot: {e}")

    def verify_ttl_index(self, collection_name: str, index_name: str):
        try:
            collection = self.get_collection(collection_name)
            index_info = collection.index_information()
            for i_name, i_spec in index_info.items():
                if (
                    i_spec.get("expireAfterSeconds") is not None
                    and index_name in i_name
                ):
                    return True
            return False

        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error verifying TTL index: {e}")
            return False

    def create_ttl_index(
        self,
        collection_name: str,
        index_name: str,
        ascending: bool,
        expire_after_seconds: int,
    ):
        try:
            collection = self.get_collection(collection_name)
            if not self.verify_ttl_index(collection_name, index_name):
                direction = pymongo.ASCENDING if ascending else pymongo.DESCENDING
                collection.create_index(
                    [(index_name, direction)],
                    expireAfterSeconds=expire_after_seconds,
                )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error creating TTL index: {e}")

    # Users
    def add_user(self, user: User) -> None:
        """TODO: Document method"""
        try:
            collection = self.get_collection("user_data")
            collection.insert_one(
                {
                    "login": user.login,
                    "hashed_password": user.hashed_password,
                    "name": user.name,
                    "api_key": user.api_key,
                    "secret_key": user.secret_key,
                    "user_settings": user.user_settings.__dict__,
                    "usd_balance": user.usd_balance,
                    "created_at": user.created_at,
                }
            )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error adding user {user.login} to database: {e}")

    def get_users(self, query: dict = {}) -> List[User]:
        """TODO: Document method"""
        try:
            collection = self.get_collection("user_data")
            cursor = collection.find(query)
            users = []
            for document in cursor:
                user = User(
                    id=document["_id"],
                    login=document["login"],
                    hashed_password=document["hashed_password"],
                    name=document["name"],
                    api_key=document["api_key"],
                    secret_key=document["secret_key"],
                    user_settings=UserSettings(**document["user_settings"]),
                    usd_balance=document["usd_balance"],
                    created_at=document["created_at"],
                )
                users.append(user)
            return users if len(users) > 1 else users[0] if users else None
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error getting users data: {e}")
            return None

    # Assets
    def add_asset(self, asset: Asset):
        """TODO: Document method"""
        try:
            collection = self.get_collection("assets")
            collection.insert_one(
                {
                    "user_id": asset.user_id,
                    "symbol": asset.symbol,
                    "quantity": asset.quantity,
                    "purchase_price": asset.purchase_price,
                    "purchase_datetime": asset.purchase_datetime,
                    "highest_price": asset.highest_price,
                    "current_price": asset.current_price,
                    "variation": asset.variation,
                    "should_be_sold": asset.should_be_sold,
                    "obs": asset.obs,
                }
            )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(
                f"Error adding asset {asset.symbol} to {user_id} into database: {e}"
            )

    def get_assets(self, query: dict = {}) -> List[Asset]:
        """TODO: Document method"""
        try:
            collection = self.get_collection("assets")
            cursor = collection.find(query)
            assets = []
            for document in cursor:
                asset = Asset(
                    user_id=document["user_id"],
                    symbol=document["symbol"],
                    quantity=document["quantity"],
                    purchase_price=document["purchase_price"],
                    purchase_datetime=document["purchase_datetime"],
                    highest_price=document["highest_price"],
                    current_price=document["current_price"],
                    should_be_sold=document["should_be_sold"],
                    obs=document["obs"])
                assets.append(asset)
            return assets
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error getting assets data: {e}")
            return []

    def _update_asset(self, asset: Asset):
        """TODO: Document method"""
        try:
            collection = self.get_collection("assets")
            updated_asset = asset.__dict__
            collection.update_one(
                {"user_id": asset.user_id, "symbol": asset.symbol},
                {"$set": updated_asset},
            )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(
                f"Error updating asset {asset.symbol} for user {asset.user_id}: {e}"
            )

    def update_assets(self, crypto_snapshots: List[CryptoSnapshot]) -> List[Asset]:
        assets = self.get_assets()
        crypto_snapshots_dict = {
            crypto_snapshot.symbol: crypto_snapshot
            for crypto_snapshot in crypto_snapshots
        }
        updated_assets = []
        for asset in assets:
            if asset.symbol in crypto_snapshots_dict:
                updated_asset = asset.update_asset(crypto_snapshots_dict[asset.symbol].price)
                self._update_asset(updated_asset)
                updated_assets.append(updated_asset)
        return updated_assets










