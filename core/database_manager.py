import pandas as pd
import os
from dotenv import load_dotenv
import pymongo
import logging
from bson.timestamp import Timestamp
from typing import List

from models.crypto_snapshot import CryptoSnapshot
from models.user import User
from models.user_settings import UserSettings
from datetime import datetime, timedelta


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
                "timestamp": crypto_snapshot.timestamp,
                "price": crypto_snapshot.price,
            },
        )

        return operation

    def delete_crypto_snapshots(
        self,
        symbol: str,
    ) -> None:
        """TODO: Document method"""
        try:
            deletion_timestamp = Timestamp(
                int((datetime.now() - timedelta(days=1)).timestamp()), 0
            )
            collection = self.get_collection("crypto_snapshots")
            collection.update_one(
                {"symbol": symbol},
                {"$pull": {"snapshots": {"timestamp": {"$lt": deletion_timestamp}}}},
            )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error deleting snapshot from {symbol}: {e}")

    def get_all_crypto_snapshots(self) -> List[pd.DataFrame]:
        """
        Retrieves price data for all USD symbols as a list of DataFrames.

        Returns:
            list: List of DataFrames containing crypto data for each symbol.
        """
        try:
            collection = self.get_collection("crypto_snapshots")
            cursor = collection.find()
            dataframe_list = []
            for document in cursor:
                snap = pd.DataFrame(document)
                dataframe_list.append(snap)
            return dataframe_list
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error getting crypto snapshot: {e}")

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
                    "tier": user.tier,
                    "api_key": user.api_key,
                    "secret_key": user.secret_key,
                    "user_settings": user.user_settings.__dict__,
                    "assets": user.assets,
                    "created_at": user.created_at,
                }
            )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error adding user {user.login} to database: {e}")

    def get_all_users(self, query: dict = {}) -> List[User]:
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
                    tier=document["tier"],
                    api_key=document["api_key"],
                    secret_key=document["secret_key"],
                    user_settings=UserSettings(**document["user_settings"]),
                    assets=document["assets"],
                    created_at=document["created_at"],
                )
                users.append(user)
            return users
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Erro ao obter dados dos usuários: {e}")
            return []
