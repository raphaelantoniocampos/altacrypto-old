import pandas as pd
import os
from dotenv import load_dotenv
import pymongo
import logging
import bson
from typing import List

from datetime import datetime
from models.crypto_snapshot import CryptoSnapshot
from models.user import User, UserSettings
from models.asset import Asset
from global_settings import GlobalSettings


class DatabaseManager:
    """
    Manages interactions with the database.

    Attributes:
        dbname (pymongo.database.Database): The database instance.
        logger (logging.Logger): Logger object for logging messages.
    """

    def __init__(self):
        """
        Initializes the DatabaseManager.

        Retrieves database credentials from environment variables and establishes connection.
        """
        self.dbname = self.get_database()
        self.logger = logging.getLogger(__name__)

    def get_database(self) -> pymongo.database.Database:
        """
        Establishes connection to the MongoDB database.

        Returns:
            pymongo.database.Database: The MongoDB database instance.
        """
        load_dotenv()
        mongo_user = os.getenv("MONGO_USER")
        mongo_password = os.getenv("MONGO_PASSWORD")
        connection_string = f"mongodb+srv://{mongo_user}:{
            mongo_password}@cluster0.wovexfa.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0"
        client: pymongo.MongoClient = pymongo.MongoClient(connection_string)
        return client["altadata"]

    def get_collection(self, collection_name: str) -> pymongo.collection.Collection:
        """
        Retrieves a specific collection from the database.

        Args:
            collection_name (str): The name of the collection.

        Returns:
            pymongo.collection.Collection: The collection instance.
        """
        return self.dbname[f"{collection_name}"]

    def verify_ttl_index(self, collection_name: str, index_name: str):
        """
        Verifies the existence of a TTL index on a collection.

        Args:
            collection_name (str): The name of the collection.
            index_name (str): The name of the TTL index.

        Returns:
            bool: True if the TTL index exists, False otherwise.
        """
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
    ) -> None:
        """
        Creates a TTL index on a collection.

        Args:
            collection_name (str): The name of the collection.
            index_name (str): The name of the TTL index.
            ascending (bool): True if index should be ascending, False for descending.
            expire_after_seconds (int): Number of seconds before documents expire.
        """
        try:
            collection = self.get_collection(collection_name)
            direction = pymongo.ASCENDING if ascending else pymongo.DESCENDING
            collection.create_index(
                [(index_name, direction)],
                expireAfterSeconds=expire_after_seconds,
            )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error creating TTL index: {e}")

    # CryptoSnapshots
    def feed_database(self, crypto_snapshots: List[CryptoSnapshot]) -> None:
        """
        Updates the database with crypto snapshots.

        Args:
        crypto_snapshots (List['CryptoSnapshot']): containing asset pairs and their prices.
        """
        if not crypto_snapshots:
            return
        try:
            collection = self.get_collection("crypto_snapshots")
            if not self.verify_ttl_index("crypto_snapshots", "datetime"):
                self.create_ttl_index(
                    collection_name="crypto_snapshots",
                    index_name="datetime",
                    ascending=False,
                    expire_after_seconds=86400,
                )
            bulk_operations = []
            for crypto_snapshot in crypto_snapshots:
                operation = self._create_add_crypto_snapshot_operation(
                    crypto_snapshot)
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
        """
        Creates an operation to add a crypto snapshot to the database.

        Args:
            crypto_snapshot (CryptoSnapshot): The crypto snapshot to be added.

        Returns:
            pymongo.operations.UpdateOne | pymongo.operations.InsertOne: The operation to add the snapshot.
        """
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
            return []

    # Users
    def add_user(self, user: User) -> None:
        """
        Adds a user to the database.

        Args:
            user (User): The user object to be added.
        """
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
            self.logger.info(f"Error adding user {
                             user.login} to database: {e}")

    def get_users(self, query: dict = {}) -> List[User] | None:
        """
        Retrieves user data from the database based on a query.

        Args:
            query (dict): The query to filter users.

        Returns:
            List[User] | None: A list of User objects matching the query, or None if no users found.
        """
        try:
            collection = self.get_collection("user_data")
            cursor = collection.find(query)
            users = []
            for document in cursor:
                user = User(
                    _id=document["_id"],
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
            return users if users else None
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error getting users data: {e}")
            return None

    def update_user(self, user: User) -> None:
        """
        Updates the USD balance of a user in the database.

        Args:
            user (User): The user object containing updated USD balance.
        """
        try:
            collection = self.get_collection("user_data")
            collection.update_one(
                {"_id": user._id},
                {"$set": {"usd_balance": user.usd_balance}},
            )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error updating user {user._id}: {e}")

    # Assets
    def add_asset(self, asset: Asset) -> None:
        """
        Adds an asset to the database.

        Args:
            asset (Asset): The asset object to be added.
        """
        try:
            collection = self.get_collection("assets")
            if not self.verify_ttl_index("assets", "sold"):
                self.create_ttl_index(
                    collection_name="assets",
                    index_name="sold",
                    ascending=False,
                    expire_after_seconds=(
                        GlobalSettings.INTERVALS_IN_MINUTES[-1] * 60)
                )
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
                    "sold": asset.sold,
                }
            )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(
                f"Error adding asset {asset.symbol} to {
                    asset.user_id} into database: {e}"
            )

    def get_assets(self, query: dict = {}) -> List[Asset]:
        """
        Retrieves asset data from the database based on a query.

        Args:
            query (dict): The query to filter assets.

        Returns:
            List[Asset]: A list of Asset objects matching the query.
        """
        try:
            collection = self.get_collection("assets")
            cursor = collection.find(query)
            assets = []
            for document in cursor:
                asset = Asset(
                    _id=document["_id"],
                    user_id=document["user_id"],
                    symbol=document["symbol"],
                    quantity=document["quantity"],
                    purchase_price=document["purchase_price"],
                    purchase_datetime=document["purchase_datetime"],
                    highest_price=document["highest_price"],
                    current_price=document["current_price"],
                    should_be_sold=document["should_be_sold"],
                    sold=document["sold"])
                assets.append(asset)
            return assets
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error getting assets data: {e}")
            return []

    def _update_asset(self, asset: Asset) -> None:
        """
        Updates an asset in the database.

        Args:
            asset (Asset): The asset object to be updated.
        """
        try:
            collection = self.get_collection("assets")
            updated_asset = asset.__dict__
            collection.update_one(
                {"_id": asset._id},
                {"$set": updated_asset},
            )
        except pymongo.errors.PyMongoError as e:
            self.logger.info(
                f"Error updating asset {
                    asset.symbol} for user {asset.user_id}: {e}"
            )

    def update_assets(self, crypto_snapshots: List[CryptoSnapshot]) -> List[Asset]:
        """
        Updates asset information in the database based on crypto snapshots.

        Args:
            crypto_snapshots (List[CryptoSnapshot]): A list of CryptoSnapshot objects.

        Returns:
            List[Asset]: A list of updated Asset objects.
        """
        try:
            assets = self.get_assets({"sold": None})
            crypto_snapshots_dict = {
                crypto_snapshot.symbol: crypto_snapshot
                for crypto_snapshot in crypto_snapshots
            }
            updated_assets = []
            for asset in assets:
                if asset.symbol in crypto_snapshots_dict:
                    updated_asset = asset.update_asset(
                        crypto_snapshots_dict[asset.symbol].price)
                    self._update_asset(updated_asset)
                    updated_assets.append(updated_asset)
            return updated_assets
        except pymongo.errors.PyMongoError as e:
            self.logger.info(
                f"Error updating assets: {e}"
            )
            return []

    def update_sold_asset(self, asset_id: bson.objectid.ObjectId, current_datetime: datetime) -> None:
        """
        Updates the sold datetime of an asset in the database.

        Args:
            asset_id (bson.objectid.ObjectId): The ID of the asset to be updated.
            current_datetime (datetime): The datetime indicating when the asset was sold.
        """
        try:
            collection = self.get_collection("assets")
            collection.update_one({"_id": asset_id}, {
                                  "$set": {"sold": current_datetime}})
        except pymongo.errors.PyMongoError as e:
            self.logger.info(f"Error updating sold asset {asset_id}: {e}")
