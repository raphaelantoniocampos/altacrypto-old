class AssetAnalyzer:
    @staticmethod
    def fetch_and_analyze_assets_data():
        """
        Fetch price data, update the database, analyze assets, and generate recommendations.
        """
        asset_pairs = binance_api.fetch_usdt_pairs()
        current_timestamp = get_current_timestamp()
        
        database_feeder.update_database(asset_pairs, current_timestamp)
        evaluate_assets(asset_pairs)

    @staticmethod
    def evaluate_assets(asset_pairs):
        """
        Analyze existing assets and make sell decisions if necessary.

        Args:
            asset_pairs (DataFrame): DataFrame containing asset pairs data.
        """
        purchase_recommendations = identify_purchase_recommendations(asset_pairs)
        assets_dataframe = data_manager.get_assets_dataframe()

        if not assets_dataframe.empty:
            for _, row in assets_dataframe.iterrows():
                asset = Asset.from_series(row)
                if asset.symbol != 'USDT':
                    asset.update_asset(float(asset_pairs.loc[asset_pairs['symbol'] == asset.symbol, 'price'].iloc[0]))
                    data_manager.update_asset(asset)
                    if should_asset_be_sold(asset, purchase_recommendations):
                        sell_asset(asset)

        execute_purchase_recommendations(purchase_recommendations, assets_dataframe)

    @staticmethod
    def should_asset_be_sold(asset, purchase_recommendations):
        """
        Determine if an asset should be sold based on purchase recommendations and predefined rules.

        Args:
            asset (Asset): The asset to be evaluated.
            purchase_recommendations (DataFrame): DataFrame containing purchase recommendations.

        Returns:
            bool: True if the asset should be sold, False otherwise.
        """
        recommended_purchase_symbols = set(purchase_recommendations['symbol'])
        if asset.symbol in recommended_purchase_symbols:
            pass
            #return False
        if asset.variation <= (- UNDER_PURCHASE_PERCENTAGE):
            return True
        #if asset.current_price > asset.purchase_price:
        if asset.current_price <= asset.highest_price * (1 - UNDER_HIGHEST_PERCENTAGE / 100):
            return True
        if asset.variation >= ABOVE_PURCHASE_PERCENTAGE:
            return True
        return False

    @staticmethod
    def identify_purchase_recommendations(asset_pairs):
        """
        Identify purchase recommendations based on price variations.

        Args:
            asset_pairs (DataFrame): DataFrame containing asset pairs data.

        Returns:
            DataFrame: DataFrame containing purchase recommendations.
        """
        current_datetime = get_datetime()
        purchase_recommendations = pd.DataFrame()
        for interval_in_minutes in INTERVAL_IN_MINUTES:
            interval_index = int(interval_in_minutes / EXECUTION_FREQUENCY_MINUTES)
            interval_dataframe = generate_price_change_data(
                interval_index, asset_pairs, current_datetime
                )
            if not interval_dataframe.empty:
                interval_recommendations = process_interval_data(interval_dataframe)
                purchase_recommendations = pd.concat([purchase_recommendations, interval_recommendations])
        return purchase_recommendations

    @staticmethod
    def execute_purchase_recommendations(purchase_recommendations, assets_dataframe):
        """
        Execute purchase recommendations by buying assets if conditions are met.

        Args:
            purchase_recommendations (DataFrame): DataFrame containing purchase recommendations.
            assets_dataframe (DataFrame): DataFrame containing asset data.
        """
        assets_symbols = set(assets_dataframe['symbol'])
        operation_value = get_operation_value()
        for _, row in purchase_recommendations.iterrows():
            symbol = row['symbol']
            if symbol not in assets_symbols:
                if has_balance(operation_value):
                    buy_asset(row, operation_value)
                    assets_symbols.add(symbol)
                else:
                    current_datetime = get_datetime()
                    transaction_data = TransactionData(
                    date=current_datetime.date(),
                    time=current_datetime.strftime('%H:%M:%S'),
                    order_type="Compra",
                    quantity=None,
                    coin=None,
                    USDT_quantity=None,
                    purchase_price=None,
                    sell_price=None,
                    profit_loss=None,
                    variation=None,
                    interval=None,
                    trading_fee=None,
                    USDT_balance=None,
                    final_balance=balance
                )
                    log_asset_transaction(message=message)

    @staticmethod
    def process_interval_data(interval_dataframe):
        """
        Process interval data and generate purchase recommendations.

        Args:
            interval_dataframe (DataFrame): DataFrame containing price variations within an interval.

        Returns:
            DataFrame: DataFrame containing purchase recommendations.
        """
        mean_variation = interval_dataframe['variation'].mean()
        interval_recommendations = interval_dataframe[
            interval_dataframe['variation'] >= mean_variation + PERCENTAGE_THRESHOLD
            ]
        return interval_recommendations

    @staticmethod
    def generate_price_change_data(interval_index, usdt_pairs, current_datetime):
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

        for df in data_manager.get_all_coins_dataframes(usdt_pairs):
            if len(df) > interval_index:
                last_entry = df.iloc[0]
                past_entry = df.iloc[interval_index]
                current_price = last_entry['price']
                past_price = past_entry['price']
                interval_time = pd.to_datetime((last_entry['timestamp'] - past_entry['timestamp']), unit='s').strftime("%H:%M:%S")            
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
