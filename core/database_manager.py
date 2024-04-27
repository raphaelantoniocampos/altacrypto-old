import pandas as pd
import os
from dotenv import load_dotenv
import pymongo
import logging
from bson.timestamp import Timestamp
from typing import List

from models.crypto_data import CryptoData
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

    # CryptoData
    def feed_database(self, crypto_data_list: List[CryptoData]) -> None:
        """
        Updates the database with crypto data.

        Args:
        crypto_data_list (List['CryptoData']): containing asset pairs and their prices.
        """
        try:
            collection = self.get_collection("crypto_data")
            collection.create_index(
                [("symbol", pymongo.ASCENDING)], name="symbol_index"
            )

            bulk_operations = []
            for crypto_data in crypto_data_list:
                operation = self.create_add_crypto_data_operation(crypto_data)
                bulk_operations.append(operation)

            if bulk_operations:
                collection.bulk_write(bulk_operations)

            current_datetime = datetime.now()
            self.logger.info(f"Crypto Data updated at {current_datetime}")

        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error adding feeding database: {e}")

    def create_add_crypto_data_operation(
        self, crypto_data: CryptoData
    ) -> pymongo.operations.UpdateOne | pymongo.operations.InsertOne:
        """TODO: Document method"""
        collection = self.get_collection("crypto_data")
        document = collection.find_one({"symbol": crypto_data.symbol})
        if document:
            operation = pymongo.UpdateOne(
                {"symbol": document.get("symbol")},
                {
                    "$push": {
                        "snapshots": {
                            "timestamp": crypto_data.snapshots[0]["timestamp"],
                            "price": crypto_data.snapshots[0]["price"],
                        }
                    }
                },
            )
        else:
            operation = pymongo.InsertOne(
                {
                    "symbol": crypto_data.symbol,
                    "snapshots": crypto_data.snapshots,
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
            collection = self.get_collection("crypto_data")
            collection.update_one(
                {"symbol": symbol},
                {"$pull": {"snapshots": {"timestamp": {"$lt": deletion_timestamp}}}},
            )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error deleting snapshot from {symbol}: {e}")

    def get_crypto_data(self, symbol: str) -> pd.DataFrame:
        """
        Retrieves price data for a USD symbol as a DataFrame.

        Args:
            symbol (str): Symbol of the document to fetch data from.

        Returns:
            pd.DataFrame: DataFrame containing price data.
        """
        collection = self.get_collection("crypto_data")
        document = collection.find_one({"symbol": symbol})
        return pd.DataFrame(document)

    def get_all_crypto_data(self) -> List[pd.DataFrame]:
        """
        Retrieves price data for all USD symbols as a list of DataFrames.

        Returns:
            list: List of DataFrames containing crypto data for each symbol.
        """
        try:
            collection = self.get_collection("crypto_data")
            cursor = collection.find()
            dataframe_list = []
            for document in cursor:
                snap = pd.DataFrame(document)
                dataframe_list.append(snap)
            return dataframe_list
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error getting crypto data: {e}")

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
