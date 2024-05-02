import logging
from pprint import pprint
from models.crypto_snapshot import CryptoSnapshot
import asyncio
import sys
from datetime import datetime, timedelta
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
        self.database_manager = database_manager
        self.logger = logging.getLogger(__name__)

    def start(self) -> None:
        """TODO: Document method"""
        binance_manager = BinanceManager()

        crypto_snapshots = binance_manager.fetch_usdt_pairs()
        self.database_manager.feed_database(crypto_snapshots)

        users = self.database_manager.get_all_users()

        intervals_dataframe = self._get_intervals_dataframes()
        purchase_recommendations = self._identify_purchase_recommendations(
            intervals_dataframe, GlobalSettings.STANDARD_USER_SETTINGS
        )
        pprint(purchase_recommendations)
        sys.exit()

    def _get_intervals_dataframes(self) -> List[pd.DataFrame]:
        """TODO: Document method"""
        intervals_dataframe = []
        all_crypto_snapshots = self.database_manager.get_all_crypto_snapshots()
        crypto_snapshots_by_symbol = self._separate_crypto_snapshots_by_symbol(
            all_crypto_snapshots
        )
        current_datetime = datetime.now()
        for interval_in_minutes in GlobalSettings.INTERVALS_IN_MINUTES:
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
                    if isinstance(past_snapshot, pd.DataFrame):
                        past_snapshot = past_snapshot.iloc[-1]
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
                    # self.logger.info(f"Error getting interval dataframe: {e}")
                    variation_data.append({})
            interval_dataframe = pd.DataFrame(variation_data)
            df = interval_dataframe
            if not interval_dataframe.empty:
                intervals_dataframe.append(interval_dataframe)
        return intervals_dataframe

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

    def _identify_purchase_recommendations(
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
                ]
                if not interval_recommendations.empty:
                    interval_recommendations["mean_variation"] = mean_variation
                    purchase_recommendations = pd.concat(
                        [purchase_recommendations, interval_recommendations]
                    )
        return purchase_recommendations

    def _evaluate_assets(self, user: User) -> None:
        """TODO: Document method"""
        assets_dataframe = self.database_manager.get_assets_dataframe()

        if not assets_dataframe.empty:
            for _, row in assets_dataframe.iterrows():
                asset = Asset.from_series(row)
                if asset.symbol != "USDT":
                    asset.update_asset(
                        float(
                            asset_pairs.loc[
                                asset_pairs["symbol"] == asset.symbol, "price"
                            ].iloc[0]
                        )
                    )
                    self.data_manager.update_asset(asset)
                    if self._should_asset_be_sold(asset):
                        TransactionManager.sell_asset(
                            asset,
                            self.data_manager,
                            self.user_settings,
                        )

        self._execute_purchase_recommendations(
            purchase_recommendations, assets_dataframe
        )

    def _should_asset_be_sold(self, asset: Asset) -> bool:
        """
        Determine if an asset should be sold based on purchase recommendations and predefined rules.

        Args:
            asset (Asset): The asset to be evaluated.
            purchase_recommendations (DataFrame): DataFrame containing purchase recommendations.

        Returns:
            bool: True if the asset should be sold, False otherwise.
        """
        if asset.variation <= (-self.user_settings.under_purchase_percentage):
            return True
        if asset.current_price <= asset.highest_price * (
            1 - self.user_settings.under_highest_percentage / 100
        ):
            return True
        if asset.variation >= self.user_settings.above_purchase_percentage:
            return True
        return False

    def _execute_purchase_recommendations(
        self, purchase_recommendations: pd.DataFrame, assets_dataframe: pd.DataFrame
    ) -> None:
        """
        Execute purchase recommendations by buying assets if conditions are met.

        Args:
            purchase_recommendations (DataFrame): DataFrame containing purchase recommendations.
            assets_dataframe (DataFrame): DataFrame containing asset data.
        """
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


