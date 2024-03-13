import pandas as pd

from models.asset import Asset
import utils.settings as settings
import utils.datetime_utils as datetime_utils
from managers.balance_manager import BalanceManager
from managers.transaction_manager import TransactionManager


class AssetAnalyzer:
    def __init__(self, binance_manager, data_manager):
        self.binance_manager = binance_manager
        self.data_manager = data_manager

    def run(self):
        """
        Fetch price data, update the database, analyze assets, and generate recommendations.
        """
        asset_pairs = self.binance_manager.fetch_usdt_pairs()
        self.data_manager.feed_database(asset_pairs)
        self._evaluate_assets(asset_pairs)

    def _evaluate_assets(self, asset_pairs):
        """
        Analyze existing assets and make sell decisions if necessary.

        Args:
            asset_pairs (DataFrame): DataFrame containing asset pairs data.
        """
        purchase_recommendations = self._identify_purchase_recommendations(asset_pairs)
        assets_dataframe = self.data_manager.get_assets_dataframe()

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
                    if self.should_asset_be_sold(asset, purchase_recommendations):
                        self.sell_asset(asset)

        self._execute_purchase_recommendations(
            purchase_recommendations, assets_dataframe
        )

    def _should_asset_be_sold(self, asset, purchase_recommendations):
        """
        Determine if an asset should be sold based on purchase recommendations and predefined rules.

        Args:
            asset (Asset): The asset to be evaluated.
            purchase_recommendations (DataFrame): DataFrame containing purchase recommendations.

        Returns:
            bool: True if the asset should be sold, False otherwise.
        """
        if asset.variation <= (-settings.UNDER_PURCHASE_PERCENTAGE):
            return True
        if asset.current_price <= asset.highest_price * (
            1 - settings.UNDER_HIGHEST_PERCENTAGE / 100
        ):
            return True
        if asset.variation >= settings.ABOVE_PURCHASE_PERCENTAGE:
            return True
        return False

    def _identify_purchase_recommendations(self, asset_pairs):
        """
        Identify purchase recommendations based on price variations.

        Args:
            asset_pairs (DataFrame): DataFrame containing asset pairs data.

        Returns:
            DataFrame: DataFrame containing purchase recommendations.
        """
        current_datetime = datetime_utils.get_datetime()
        purchase_recommendations = pd.DataFrame()
        for interval_in_minutes in settings.INTERVAL_IN_MINUTES:
            interval_index = int(
                interval_in_minutes / settings.EXECUTION_FREQUENCY_MINUTES
            )
            interval_dataframe = self.generate_price_change_data(
                interval_index, asset_pairs, current_datetime
            )
            if not interval_dataframe.empty:
                interval_recommendations = self.process_interval_data(
                    interval_dataframe
                )
                purchase_recommendations = pd.concat(
                    [purchase_recommendations, interval_recommendations]
                )
        return purchase_recommendations

    def _execute_purchase_recommendations(
        self, purchase_recommendations, assets_dataframe
    ):
        """
        Execute purchase recommendations by buying assets if conditions are met.

        Args:
            purchase_recommendations (DataFrame): DataFrame containing purchase recommendations.
            assets_dataframe (DataFrame): DataFrame containing asset data.
        """
        assets_symbols = set(assets_dataframe['symbol'])
        operation_value = BalanceManager.get_operation_value()
        for _, row in purchase_recommendations.iterrows():
            symbol = row['symbol']
            if symbol not in assets_symbols:
                has_balance = TransactionManager.has_balance(operation_value)
                if has_balance[0]:
                    TransactionManager.buy_asset(row, operation_value)
                    assets_symbols.add(symbol)
                else:
                    current_datetime = datetime_utils.get_datetime()
                    TransactionManager.attempt_purchase(
                        current_datetime, has_balance[1]
                    )

    def _process_interval_data(self, interval_dataframe):
        """
        Process interval data and generate purchase recommendations.

        Args:
            interval_dataframe (DataFrame): DataFrame containing price variations within an interval.

        Returns:
            DataFrame: DataFrame containing purchase recommendations.
        """
        mean_variation = interval_dataframe['variation'].mean()
        interval_recommendations = interval_dataframe[
            interval_dataframe["variation"]
            >= mean_variation + settings.PERCENTAGE_THRESHOLD
        ]
        return interval_recommendations

    def _generate_price_change_data(self, interval_index, usdt_pairs, current_datetime):
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
                variation_data.append({
                    'symbol': last_entry['symbol'],
                    'interval': interval_time,
                    'current_datetime': current_datetime,
                    'current_price': current_price,
                    'past_price': past_price,
                    'variation': variation_percent
                })
        return pd.DataFrame(variation_data)
