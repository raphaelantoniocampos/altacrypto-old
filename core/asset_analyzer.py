import asyncio
from datetime import datetime

import pandas as pd

from core.binance_manager import BinanceManager
from data_access.asset import Asset
from data_access.user import User
from data_access.crypto_price import CryptoPrice
from data_access.database_manager import DatabaseManager
from utils.datetime_utils import DateTimeUtils
from utils.global_settings import GlobalSettings


class AssetAnalyzer:
    """TODO: Document class."""

    def run(self) -> None:
        """
        Fetch price data, update the database, analyze assets, and generate recommendations.
        """
        binance_manager = BinanceManager()
        database_manager = DatabaseManager(GlobalSettings.CRYPTO_PRICES_DB_PATH)
        crypto_prices = binance_manager.fetch_usdt_pairs()
        CryptoPrice.feed_database(crypto_prices)
        users = User.get_all_users()
        for user in users:
            self._evaluate_assets(user)

    def _evaluate_assets(self, user: User) -> None:
        """
        Analyze existing assets and make sell decisions if necessary.

        Args:
            asset_pairs (DataFrame): DataFrame containing asset pairs data.
        """
        database_manager = DatabaseManager(GlobalSettings.USER_DATA_DB_PATH)
        purchase_recommendations = self._identify_purchase_recommendations(asset_pairs)
        assets_dataframe = database_manager.get_assets_dataframe()

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

    def _identify_purchase_recommendations(
        self, asset_pairs: pd.DataFrame
    ) -> pd.DataFrame:
        """
        Identify purchase recommendations based on price variations.

        Args:
            asset_pairs (DataFrame): DataFrame containing asset pairs data.

        Returns:
            DataFrame: DataFrame containing purchase recommendations.
        """
        current_datetime = DateTimeUtils.get_datetime()
        purchase_recommendations = pd.DataFrame()
        for interval_in_minutes in self.user_settings.interval_in_minutes:
            interval_index = int(
                interval_in_minutes / self.user_settings.execution_frequency_minutes
            )
            interval_dataframe = self._generate_price_change_data(
                interval_index, asset_pairs, current_datetime
            )
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

    def _generate_price_change_data(
        self, interval_index: int, usdt_pairs: pd.DataFrame, current_datetime: datetime
    ) -> pd.DataFrame:
        """
        Generates a DataFrame containing price variations for a given interval.

        Args:
            interval_index (int): Index of the interval.
            usdt_pairs (DataFrame): DataFrame containing USD pairs.
            current_datetime (datetime): Current datetime.

        Returns:
            DataFrame: DataFrame containing price variations.
        """
        variation_data = []
        interval_time = None

        for df in self.data_manager.get_all_coins_dataframes(usdt_pairs):
            if len(df) > interval_index:
                last_entry = df.iloc[0]
                past_entry = df.iloc[interval_index]
                current_price = last_entry["price"]
                past_price = past_entry["price"]
                interval_time = pd.to_datetime(
                    (last_entry["timestamp"] - past_entry["timestamp"]), unit="s"
                ).strftime("%H:%M:%S")
                variation_percent = ((current_price - past_price) / current_price) * 100
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

