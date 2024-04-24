import pymongo

data_agora = {"date":new Timestamp()}
print(data_agora)
print(type(data_agora))

# TESTES ACIMA #

import pandas as pd
import os
from dotenv import load_dotenv
import pymongo
from data_access.crypto_price import CryptoPrice


class DatabaseManager:
    """TODO: Document class"""

    def __init__(self):
        """TODO: Document method"""
        self.dbname = self.get_database()

    def get_database(self) -> pymongo.MongoClient:
        load_dotenv()
        mongo_user = os.getenv("MONGO_USER")
        mongo_password = os.getenv("MONGO_PASSWORD")
        connection_string = f"mongodb+srv://{mongo_user}:{mongo_password}@cluster0.wovexfa.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0"
        client = pymongo.MongoClient(connection_string)
        return client["altadata"]

    def get_collection(self, collection_name: str) -> pymongo.collection.Collection:
        """TODO: Document method"""
        return self.dbname[f"{collection_name}"]

    def create_crypto_price(self, crypto_price: CryptoPrice) -> None:
        """TODO: Document method"""
        try:
            collection_name = self.get_collection("crypto_prices")
            document = collection_name.find_one({"symbol": self.symbol})
            if document:
                collection_name.update_one(
                    {"_id": document.get("_id")},
                    {"$push": {"snapshots": self.snapshot}},
                )
                self.delete_from_database()
            else:
                collection_name.insert_one(
                    {"symbol": self.symbol, "snapshots": [self.snapshot]}
                )
        except pymongo.errors.PyMongoError as e:
            # TODO: Change to logging
            print(f"Error adding crypto_price to database: {e}")

        """
        deletion_timestamp = int(
            (datetime.fromtimestamp(self.timestamp) - timedelta(days=1)).timestamp()
        )
        self.delete_from_database(table_name, deletion_timestamp)
        """

    def delete_crypto_price(
        self,
        symbol: str,
        deletion_timestamp: int,
    ) -> None:
        """TODO: Document method"""
        pass

    def _get_coin_prices_dataframe(self, table_name: str) -> pd.DataFrame:
        """
        Retrieves price data for a USD symbol as a DataFrame.

        Args:
            table_name (str): Name of the table to fetch data from.

        Returns:
            pd.DataFrame: DataFrame containing price data.
        """
        table_name = self.database_manager.format_symbol(table_name)
        if not self.database_manager.table_exists(table_name):
            self._create_crypto_price_table(table_name)
        sql = f"SELECT timestamp, price FROM {table_name}"
        df = self.database_manager._execute_sql(
            sql, f"Error selecting from {table_name}", table_name=table_name
        )
        if df is not None:
            df["symbol"] = self.database_manager.format_symbol(table_name)
        return df

    def get_all_coins_dataframes(self, usdt_pairs: pd.DataFrame) -> list[pd.DataFrame]:
        """
        Retrieves price data for all USD symbols as a list of DataFrames.

        Args:
            usdt_pairs (pd.DataFrame): DataFrame containing USD symbol pairs.

        Returns:
            list: List of DataFrames containing price data for each symbol.
        """
        return [
            self._get_coin_prices_dataframe(row["symbol"])
            for _, row in usdt_pairs.iterrows()
        ]

    @staticmethod
    def feed_database(crypto_prices: list["CryptoPrice"]) -> None:
        """
        Updates the database with price snapshots for asset pairs.

        Args:
            crypto_prices (list['CryptoPrice']): containing asset pairs and their prices.
            current_timestamp (int): current timestamp integer
        """
        for crypto_price in crypto_prices:
            crypto_price.add_to_database()

        current_datetime = DateTimeUtils.get_datetime()
        GlobalSettings.logger.info(f"USD prices updated at {current_datetime}")

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


database = DatabaseManager()
collection_name = database.get_collection("crypto_prices")
serie = {"timestamp": 171335362, "price": 63122.67}
serie1 = {"timestamp": 173334762, "price": 41223.67}
serie2 = {"timestamp": 21545465, "price": 545422.67}
serie3 = {"timestamp": 44564231, "price": 87422.67}
serie4 = {"timestamp": 41245465, "price": 85422.67}
item_3 = {"symbol": "BTCUSDT", "snapshots": [serie, serie1, serie2, serie3, serie4]}

# collection_name.insert_one(item_3)

"""
cursor = collection_name.find({})
for document in cursor:
    symbol = document.get("symbol")
    series = document.get("series", [])
    # print(f"Symbol: {symbol}")
    # print("Series:")
    for serie in series:
        timestamp = serie.get("timestamp")
        price = serie.get("price")
        # print(f"Timestamp: {timestamp}, Price: {price}")
    print()  # Add a blank line between documents

filter_query = {"symbol": "BTCUSDT"}
new_serie = {"timestamp": 123456789, "price": 9876.54}
update_operation = {"$push": {"series": new_serie}}
result = collection_name.update_many(filter_query, update_operation)
if result.modified_count > 0:
    print("New series added successfully!")
else:
    print("No document matched the filter query.")
"""


