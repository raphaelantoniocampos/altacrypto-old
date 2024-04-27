import asyncio
from datetime import datetime

import pandas as pd

from models.asset import Asset

from models.user import User
from utils.global_settings import GlobalSettings
from core.database_manager import DatabaseManager
from core.binance_manager import BinanceManager


class CryptoTrader:
    """TODO: Document class."""

    def __init__(self, database_manager: DatabaseManager):
        self.database_manager = database_manager

    def start(self) -> None:
        """TODO: Document method"""
        binance_manager = BinanceManager()

        crypto_data_list = binance_manager.fetch_usdt_pairs()
        self.database_manager.feed_database(crypto_data_list)

        users = self.database_manager.get_all_users()
        for user in users:
            self._evaluate_assets(user)

    def _evaluate_assets(self, user: User) -> None:
        """TODO: Document method"""
        purchase_recommendations = self._identify_purchase_recommendations()
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

    def _identify_purchase_recommendations(self: pd.DataFrame) -> pd.DataFrame:
        """
        Identify purchase recommendations based on price variations.

        Returns:
            DataFrame: DataFrame containing purchase recommendations.
        """
        purchase_recommendations = pd.DataFrame()
        for interval in GlobalSettings.INTERVALS:
            interval_index = int(
                interval / GlobalSettings.EXECUTION_FREQUENCY_MINUTES)
            interval_dataframe = self._generate_price_change_data(
                interval_index)
            if not interval_dataframe.empty:
                interval_recommendations = self._process_interval_data(
                    interval_dataframe
                )
                purchase_recommendations = pd.concat(
                    [purchase_recommendations, interval_recommendations]
                )
        return purchase_recommendations

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

    def _process_interval_data(self, interval_dataframe: pd.DataFrame) -> pd.DataFrame:
        """
        Process interval data and generate purchase recommendations.

        Args:
            interval_dataframe (DataFrame): DataFrame containing price variations within an interval.

        Returns:
            DataFrame: DataFrame containing purchase recommendations.
        """
        mean_variation = interval_dataframe["variation"].mean()
        interval_recommendations = interval_dataframe[
            interval_dataframe["variation"]
            >= mean_variation + self.user_settings.percentage_threshold
        ]
        return interval_recommendations

    def _generate_price_change_data(self, interval_index: int) -> pd.DataFrame:
        """
        Generates a DataFrame containing price variations for a given interval.

        Args:
            interval_index (int): Index of the interval.

        Returns:
            DataFrame: DataFrame containing price variations.
        """
        variation_data = []
        interval_time = None
        current_datetime = datetime.now()
        for df in self.database_manager.get_all_crypto_data():
            if len(df) > interval_index:
                last_entry = df.iloc[0]
                past_entry = df.iloc[interval_index]
                current_price = last_entry["snapshots"].get("price")
                past_price = past_entry["snapshots"].get("price")
                interval_time = pd.to_datetime(
                    (
                        last_entry["snapshots"].get("timestamp")
                        - past_entry["snapshots"].get("timestamp")
                    ),
                    unit="s",
                ).strftime("%H:%M:%S")
                variation_percent = (
                    (current_price - past_price) / current_price) * 100
                variation_data.append(
                    {
                        "symbol": last_entry["symbol"],
                        "interval": interval_time,
                        "current_datetime": current_datetime,
                        "current_price": current_price,
                        "past_price": past_price,
                        "variation": variation_percent,
                    }
                )
        return pd.DataFrame(variation_data)
