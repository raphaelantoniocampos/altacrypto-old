import asyncio
import time

import logging
from datetime import datetime
from typing import List

import pandas as pd

from models.crypto_snapshot import CryptoSnapshot
from models.asset import Asset
from models.user import User
from models.order import Order, SellOrder, BuyOrder
from utils.global_settings import GlobalSettings
from core.database_manager import DatabaseManager
from core.binance_manager import BinanceManager


class CryptoTrader:
    """TODO: Document class."""

    def __init__(self, database_manager: DatabaseManager):
        """TODO: Document method."""
        self.database_manager = database_manager
        self.logger = logging.getLogger(__name__)

    async def start(self) -> None:
        """TODO: Document method"""
        binance_manager = BinanceManager()

        crypto_snapshots = binance_manager.fetch_usdt_pairs()
        self.database_manager.feed_database(crypto_snapshots)
        assets = self.database_manager.update_assets(crypto_snapshots)
        intervals_dataframe = self._get_intervals_dataframes()

        # users = self.database_manager.get_all_users()

        current_datetime = datetime.now()
        sell_orders = self._get_sell_orders(assets, crypto_snapshots, current_datetime)
        buy_orders = self._get_buy_orders(intervals_dataframe)

        if sell_orders or buy_orders:
            self._execute_orders(sell_orders, buy_orders)

    def _get_intervals_dataframes(self) -> List[pd.DataFrame]:
        """TODO: Document method"""
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
        """TODO: Document method"""
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
        """TODO: Document method"""
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

    def _get_buy_orders(self, intervals_dataframes: List[pd.DataFrame]) -> List[Order]:
        """TODO: Document method"""
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
        if symbols_set:
            for symbol in symbols_set:
                self.database_manager.delete_crypto_snapshots_by_symbol(symbol)
        return orders

    def _get_sell_orders(
        self,
        assets: List[Asset],
        crypto_snapshots: List[CryptoSnapshot],
        current_datetime: datetime,
    ) -> List[Order]:
        """TODO: Document method"""
        orders = []
        for asset in assets:
            if asset.should_be_sold:
                interval = ((current_datetime - asset.purchase_datetime),)
                order = SellOrder("sell", interval, asset)
                orders.append(order)
        return orders

    def _execute_orders(
        self, sell_orders: List[Order], buy_orders: List[Order]
    ) -> None:
        """TODO: Change to binance account"""
        current_datetime = datetime.now()
        users = {user._id: user for user in self.database_manager.get_users()}
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
                tasks.append(self._execute_sell_orders(user, orders, user_assets))
        asyncio.gather(*tasks)

    def _separate_orders_by_user(self, orders: List[Order]) -> dict:
        """Separates orders by user_id"""
        orders_by_user = {}
        for order in orders:
            if order.user_id not in orders_by_user:
                orders_by_user[order.user_id] = []
            orders_by_user[order.user_id].append(order)
        return orders_by_user

    async def _execute_buy_orders(
        self,
        user: User,
        buy_orders: List[Order],
        current_datetime: datetime,
        operation_value: float,
        user_assets: dict,
    ):
        for order in buy_orders:
            if user.name == "Logger":
                self.logger.info(f"{order}{datetime.now()}")
            if order.symbol not in user_assets:
                if user.usd_balance >= operation_value:
                    quantity = operation_value / order.current_price
                    asset = Asset(
                        user_id=user._id,
                        symbol=order.symbol,
                        quantity=quantity,
                        purchase_price=order.current_price,
                        purchase_datetime=current_datetime,
                        highest_price=order.current_price,
                        current_price=order.current_price,
                    )
                    # Simulates binance transaction
                    time.sleep(0.05)
                    self.database_manager.add_asset(asset)
                    user.usd_balance -= operation_value
                    self.database_manager.update_user(user)

    async def _execute_sell_orders(
        self, user: User, sell_orders: List[Order], user_assets
    ):
        for order in sell_orders:
            if user.name == "Logger":
                self.logger.info(f"{order}{datetime.now()}")
            asset = user_assets[order.asset.symbol]
            value = asset.current_value
            # Simulates binance transaction
            time.sleep(0.05)
            self.database_manager.delete_asset(asset._id)
            user.usd_balance += value
            self.database_manager.update_user(user)
