import logging
from models.crypto_snapshot import CryptoSnapshot
import asyncio
from datetime import datetime
from typing import List

import pandas as pd

from models.asset import Asset

from models.user import User
from utils.global_settings import GlobalSettings
from core.database_manager import DatabaseManager
from core.binance_manager import BinanceManager
from models.user_settings import UserSettings


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

        users = self.database_manager.get_all_users()

        intervals_dataframe = self._get_intervals_dataframes()
        purchase_recommendations = self._get_purchase_recommendations(
            intervals_dataframe, GlobalSettings.STANDARD_USER_SETTINGS
        )
        tasks = [self._evaluate_assets(user, crypto_snapshots) for user in users]
        await asyncio.gather(*tasks)

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

    def _get_purchase_recommendations(
        self, intervals_dataframes: List[pd.DataFrame], user_settings: UserSettings
    ) -> pd.DataFrame:
        """TODO: Document method"""
        purchase_recommendations = pd.DataFrame()
        for interval_dataframe in intervals_dataframes:
            if not interval_dataframe.empty:
                mean_variation = interval_dataframe["variation"].mean()
                interval_recommendations = interval_dataframe[
                    interval_dataframe["variation"]
                    >= mean_variation + user_settings.percentage_threshold
                ].copy()
                if not interval_recommendations.empty:
                    interval_recommendations["mean_variation"] = mean_variation
                    purchase_recommendations = pd.concat(
                        [purchase_recommendations, interval_recommendations]
                    )
        return purchase_recommendations

    def _execute_purchase_recommendations(
        self, purchase_recommendations: pd.DataFrame, users: List[User]
    ) -> None:
        """TODO: Document method"""
        assets_symbols = set(assets_dataframe["symbol"])
        operation_value = self.balance_manager.get_operation_value()
        for _, row in purchase_recommendations.iterrows():
            symbol = row["symbol"]
            if symbol not in assets_symbols:
                has_balance = self.balance_manager.has_balance(operation_value)
                if has_balance[0]:
                    TransactionManager.buy_asset(
                        row,
                        operation_value,
                        self.data_manager,
                        self.user_settings,
                    )
                    assets_symbols.add(symbol)
                else:
                    current_datetime = DateTimeUtils.get_datetime()
                    TransactionManager.attempt_purchase(
                        current_datetime, has_balance[1], self.user_settings
                    )

    async def _evaluate_assets(
        self, user: User, crypto_snapshots: List[CryptoSnapshot]
    ) -> None:
        """TODO: Document method"""
        assets = [Asset.from_dict(asset) for asset in user.assets]

        crypto_snapshots_dict = {
            crypto_snapshot.symbol: crypto_snapshot
            for crypto_snapshot in crypto_snapshots
        }
        if assets:
            for asset in assets:
                if asset.symbol in crypto_snapshots_dict:
                    asset.update_asset(crypto_snapshots_dict[asset.symbol].price)
                    self.database_manager.update_asset_from_user(asset, user)
                    if asset.should_be_sold:
                        print(
                            f"Selling Asset from user: {user.login}\n{asset}"
                        )  # SELL ASSET
        """

        self._execute_purchase_recommendations(
            purchase_recommendations, assets_dataframe
        )
        """
