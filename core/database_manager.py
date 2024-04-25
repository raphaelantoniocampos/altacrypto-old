import pandas as pd
import os
from dotenv import load_dotenv
import pymongo
import logging

from models.crypto_snapshot import CryptoSnapshot
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
    def add_crypto_snapshot(self, crypto_snapshot: CryptoSnapshot) -> None:
        """TODO: Document method"""
        try:
            collection = self.get_collection("crypto_snapshots")
            document = collection.find_one({"symbol": crypto_snapshot.symbol})
            if document:
                collection.update_one(
                    {"symbol": document.get("symbol")},
                    {"$push": {"snapshots": crypto_snapshot.snapshot}},
                )
                deletion_timetamp = int(
                    (datetime.now() - timedelta(days=1)).timestamp()
                )
                self.delete_crypto_snapshots(crypto_snapshot.symbol, deletion_timetamp)
            else:
                collection.insert_one(
                    {
                        "symbol": crypto_snapshot.symbol,
                        "snapshots": [crypto_snapshot.snapshot],
                    }
                )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(
                f"Error adding crypto_snapshot {crypto_snapshot.symbol} to database: {e}"
            )

    def delete_crypto_snapshots(
        self,
        symbol: str,
        deletion_timestamp: int,
    ) -> None:
        """TODO: Document method"""
        try:
            collection = self.get_collection("crypto_snapshots")
            collection.update_one(
                {"symbol": symbol},
                {"$pull": {"snapshots": {"timestamp": {"$lt": deletion_timestamp}}}},
            )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error deleting snapshot from {symbol}: {e}")

    def get_crypto_snapshot(self, symbol: str) -> pd.DataFrame:
        """
        Retrieves price data for a USD symbol as a DataFrame.

        Args:
            symbol (str): Symbol of the document to fetch data from.

        Returns:
            pd.DataFrame: DataFrame containing price data.
        """
        collection = self.get_collection("crypto_snapshots")
        document = collection.find_one({"symbol": symbol})
        return pd.DataFrame(document)

    def get_all_crypto_snapshots(self) -> list[pd.DataFrame]:
        """
        Retrieves price data for all USD symbols as a list of DataFrames.

        Args:
            usdt_pairs (pd.DataFrame): DataFrame containing USD symbol pairs.

        Returns:
            list: List of DataFrames containing price data for each symbol.
        """
        collection = self.get_collection("crypto_snapshots")
        cursor = collection.find()
        dataframe_list = []
        for document in cursor:
            snap = pd.DataFrame(document)
            dataframe_list.append(snap)
        return dataframe_list

    def feed_database(self, crypto_snapshots: list[CryptoSnapshot]) -> None:
        """
        Updates the database with crypto snapshots for asset pairs.

        Args:
            crypto_snapshots (list['CryptoSnapshot']): containing asset pairs and their prices.
        """
        for crypto_snapshot in crypto_snapshots:
            self.add_crypto_snapshot(crypto_snapshot)

        current_datetime = datetime.now()
        self.logger.info(f"USD prices updated at {current_datetime}")

    @staticmethod
    def format_symbol(symbol: str) -> str:
        """
        Adds or removes "t" prefix to symbols starting with a digit.

        Args:
            symbol (str): The symbol to format.

        Returns:
            str: The formatted symbol.
        """
        if symbol[0].isdigit():
            return "t" + symbol
        if symbol[0] == "t":
            return symbol[1:]
        return symbol

