import asyncio
import time

import logging
from datetime import datetime
from typing import List

import bson
import pandas as pd

from models.crypto_snapshot import CryptoSnapshot
from models.asset import Asset
from models.user import User
from models.order import SellOrder, BuyOrder
from global_settings import GlobalSettings
from core.database_manager import DatabaseManager
from core.binance_manager import BinanceManager


class CryptoTrader:
    """
    The class responsible for managing crypto trading operations.

    Attributes:
        database_manager (DatabaseManager): An instance of DatabaseManager for database interactions.
        logger (logging.Logger): Logger object for logging messages.
    """

    def __init__(self, database_manager: DatabaseManager):
        """
        Initializes a new instance of the CryptoTrader class.

        Args:
            database_manager (DatabaseManager): An instance of DatabaseManager for database interactions.
        """
        self.database_manager = database_manager
        self.logger = logging.getLogger(__name__)

    async def start(self) -> None:
        """
        Starts the crypto trading process.

        Fetches crypto snapshots, updates the database, analyzes data,
        and executes trading orders if conditions are met.
        """
        binance_manager = BinanceManager()

        crypto_snapshots = binance_manager.fetch_usdt_pairs()
        self.database_manager.feed_database(crypto_snapshots)
        assets = self.database_manager.update_assets(crypto_snapshots)
        intervals_dataframe = self._get_intervals_dataframes()

        # users = self.database_manager.get_all_users()

        current_datetime = datetime.now()
        sell_orders = self._get_sell_orders(
            assets, crypto_snapshots, current_datetime)
        buy_orders = self._get_buy_orders(intervals_dataframe)

        if sell_orders or buy_orders:
            self._execute_orders(sell_orders, buy_orders)

    def _get_intervals_dataframes(self) -> List[pd.DataFrame]:
        """
        Generates intervals dataframes for analysis.

        Returns:
            List[pd.DataFrame]: List of interval dataframes.
        """
        intervals_dataframe = []
        all_crypto_snapshots = self.database_manager.get_all_crypto_snapshots()
        crypto_snapshots_by_symbol = self._separate_crypto_snapshots_by_symbol(
            all_crypto_snapshots
        )
        current_datetime = datetime.now()
        for interval_in_minutes in GlobalSettings.INTERVALS_IN_MINUTES:
            interval_dataframe = self._get_interval_dataframe(
                interval_in_minutes, crypto_snapshots_by_symbol, current_datetime
            )
            if not interval_dataframe.empty:
                intervals_dataframe.append(interval_dataframe)
        return intervals_dataframe

    def _get_interval_dataframe(
        self,
        interval_in_minutes: int,
        crypto_snapshots_by_symbol: List[pd.DataFrame],
        current_datetime: datetime,
    ) -> pd.DataFrame:
        """
        Generates an interval dataframe for a given interval.

        Args:
            interval_in_minutes (int): Interval in minutes.
            crypto_snapshots_by_symbol (List[pd.DataFrame]): List of dataframes grouped by symbol.
            current_datetime (datetime): Current datetime.

        Returns:
            pd.DataFrame: Interval dataframe.
        """
        variation_data = []
        for df in crypto_snapshots_by_symbol:
            try:
                last_snapshot = df.iloc[-1]
                symbol = last_snapshot["symbol"]
                past_snapshot = df.loc[
                    (
                        df["datetime"]
                        < (
                            last_snapshot["datetime"]
                            - pd.Timedelta(minutes=(interval_in_minutes - 1))
                        )
                    )
                    & (
                        df["datetime"]
                        > (
                            last_snapshot["datetime"]
                            - pd.Timedelta(minutes=(interval_in_minutes + 1))
                        )
                    )
                ]
                if past_snapshot.empty:
                    variation_data.append(
                        {
                            "symbol": symbol,
                            "interval": None,
                            "current_datetime": current_datetime,
                            "current_price": None,
                            "past_price": None,
                            "variation": 0,
                        }
                    )
                else:
                    if isinstance(past_snapshot, pd.DataFrame):
                        past_snapshot = past_snapshot.iloc[0]
                    current_price = last_snapshot["price"]
                    past_price = past_snapshot["price"]

                    last_datetime = last_snapshot["datetime"]
                    past_datetime = past_snapshot["datetime"]

                    interval_time = last_datetime - past_datetime
                    variation_percent = (
                        (current_price - past_price) / current_price
                    ) * 100
                    variation_data.append(
                        {
                            "symbol": symbol,
                            "interval": interval_time,
                            "current_datetime": current_datetime,
                            "current_price": current_price,
                            "past_price": past_price,
                            "variation": variation_percent,
                        }
                    )
            except IndexError as e:
                self.logger.info(f"Error getting interval dataframe: {e}")
                variation_data.append(
                    {
                        "symbol": symbol,
                        "interval": None,
                        "current_datetime": current_datetime,
                        "current_price": None,
                        "past_price": None,
                        "variation": 0,
                    }
                )

        interval_dataframe = pd.DataFrame(variation_data)
        interval_dataframe = interval_dataframe[interval_dataframe["variation"] != 0.0]
        interval_dataframe.reset_index(drop=True, inplace=True)
        return interval_dataframe

    def _separate_crypto_snapshots_by_symbol(
        self, crypto_snapshots: List[CryptoSnapshot]
    ) -> List[pd.DataFrame]:
        """
        Separates crypto snapshots by symbol.

        Args:
            crypto_snapshots (List[CryptoSnapshot]): List of crypto snapshots.

        Returns:
            List[pd.DataFrame]: List of dataframes grouped by symbol.
        """
        data = []
        for crypto_snapshot in crypto_snapshots:
            data.append(
                {
                    "symbol": crypto_snapshot.symbol,
                    "datetime": crypto_snapshot.datetime,
                    "price": crypto_snapshot.price,
                }
            )
        df = pd.DataFrame(data)
        return [group for _, group in df.groupby("symbol")]

    def _get_buy_orders(self, intervals_dataframes: List[pd.DataFrame]) -> List[BuyOrder]:
        """
        Generates buy orders based on interval dataframes.

        Args:
            intervals_dataframes (List[pd.DataFrame]): List of interval dataframes.

        Returns:
            List[BuyOrder]: List of buy orders.
        """
        orders = []
        symbols_set = set()
        for interval_dataframe in intervals_dataframes:
            if not interval_dataframe.empty:
                mean_variation = interval_dataframe["variation"].mean()
                interval_recommendations = interval_dataframe[
                    interval_dataframe["variation"]
                    >= mean_variation + GlobalSettings.BUYING_PERCENTAGE_THRESHOLD
                ].copy()
                if not interval_recommendations.empty:
                    for _, row in interval_recommendations.iterrows():
                        symbol = row['symbol']
                        if symbol not in symbols_set:
                            order = BuyOrder(
                                "buy",
                                row["interval"],
                                symbol,
                                row["variation"],
                                row["current_price"],
                            )
                            symbols_set.add(symbol)
                            orders.append(order)
        return orders

    def _get_sell_orders(
        self,
        assets: List[Asset],
        crypto_snapshots: List[CryptoSnapshot],
        current_datetime: datetime,
    ) -> List[SellOrder]:
        """
        Generates sell orders based on assets and crypto snapshots.

        Args:
            assets (List[Asset]): List of assets.
            crypto_snapshots (List[CryptoSnapshot]): List of crypto snapshots.
            current_datetime (datetime): Current datetime.

        Returns:
            List[SellOrder]: List of sell orders.
        """
        orders = []
        for asset in assets:
            if asset.should_be_sold and not asset.sold:
                interval = ((current_datetime - asset.purchase_datetime),)
                order = SellOrder("sell", interval, asset)
                orders.append(order)
        return orders

    def _execute_orders(
        self, sell_orders: List[SellOrder], buy_orders: List[BuyOrder]
    ) -> None:
        """
        Executes buy and sell orders.

        Args:
            sell_orders (List[SellOrder]): List of sell orders.
            buy_orders (List[BuyOrder]): List of buy orders.
        """
        current_datetime = datetime.now()
        users_list = self.database_manager.get_users()
        if not users_list:
            return
        users = {user._id: user for user in users_list}
        sell_orders_by_user = self._separate_orders_by_user(sell_orders)
        tasks = []
        for user_id, user in users.items():
            operation_value = user.get_operation_value()
            user_assets = {
                asset.symbol: asset
                for asset in self.database_manager.get_assets({"user_id": user_id})
            }
            tasks.append(
                self._execute_buy_orders(
                    user, buy_orders, current_datetime, operation_value, user_assets
                )
            )
            if user_id in sell_orders_by_user:
                orders = sell_orders_by_user[user_id]
                tasks.append(self._execute_sell_orders(
                    user, orders, user_assets, current_datetime))
        asyncio.gather(*tasks)

    def _separate_orders_by_user(self, orders: List[SellOrder]) -> dict[bson.objectid.ObjectId | None, List[SellOrder]]:
        """
        Separates orders by user_id.

        Args:
            orders (List[SellOrder]): List of sell orders.

        Returns:
            dict[bson.objectid.ObjectId | None, List[SellOrder]]: Dictionary of sell orders grouped by user_id.
        """
        orders_by_user: dict[bson.objectid.ObjectId |
                             None, List[SellOrder]] = {}
        for order in orders:
            if order.user_id not in orders_by_user:
                orders_by_user[order.user_id] = []
            orders_by_user[order.user_id].append(order)
        return orders_by_user

    async def _execute_buy_orders(
        self,
        user: User,
        buy_orders: List[BuyOrder],
        current_datetime: datetime,
        operation_value: float,
        user_assets: dict,
    ) -> None:
        """
        Executes buy orders for a user.

        Args:
            user (User): The user for whom to execute the buy orders.
            buy_orders (List[BuyOrder]): List of buy orders.
            current_datetime (datetime): Current datetime.
            operation_value (float): Operation value.
            user_assets (dict): Dictionary of user assets.
        """
        for buy_order in buy_orders:
            if buy_order.symbol not in user_assets:
                if user.name == "Logger":
                    self.logger.info(f"{buy_order}{current_datetime}")
                if user.usd_balance >= operation_value:
                    quantity = operation_value / buy_order.current_price
                    asset = Asset(
                        user_id=user._id,
                        symbol=buy_order.symbol,
                        quantity=quantity,
                        purchase_price=buy_order.current_price,
                        purchase_datetime=current_datetime,
                        highest_price=buy_order.current_price,
                        current_price=buy_order.current_price,
                    )
                    # Simulates binance transaction
                    time.sleep(0.05)
                    self.database_manager.add_asset(asset)
                    user.usd_balance -= operation_value
                    self.database_manager.update_user(user)

    async def _execute_sell_orders(
        self, user: User, sell_orders: List[SellOrder], user_assets, current_datetime: datetime
    ) -> None:
        """
        Executes sell orders for a user.

        Args:
            user (User): The user for whom to execute the sell orders.
            sell_orders (List[SellOrder]): List of sell orders.
            user_assets: Dictionary of user assets.
            current_datetime (datetime): Current datetime.
        """
        for order in sell_orders:
            asset = user_assets[order.asset.symbol]
            if asset.sold:
                return
            if user.name == "Logger":
                self.logger.info(f"{order}{current_datetime}")
            value = asset.current_value
            # Simulates binance transaction
            time.sleep(0.05)
            self.database_manager.update_sold_asset(
                asset._id, current_datetime)
            user.usd_balance += value
            self.database_manager.update_user(user)
