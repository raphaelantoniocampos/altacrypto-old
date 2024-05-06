import datetime
import bson
from utils.global_settings import GlobalSettings


class Asset:
    """Represents an investment asset with details like symbol, quantity, prices, and performance."""

    def __init__(
        self,
        user_id: bson.objectid.ObjectId | str,
        symbol: str,
        quantity: float,
        purchase_price: float,
        purchase_datetime: datetime.datetime,
        highest_price: float,
        current_price: float,
        should_be_sold: bool = False,
        obs: str | None = None,
    ):
        """
        Initializes an Asset object with data validation.

        Args:
            symbol: String representing the asset symbol.
            quantity: Integer number of units owned.
            purchase_price: Numeric purchase price per unit.
            purchase_datetime: Datetime object of the purchase date.
            highest_price: Numeric highest price reached since purchase.
            current_price: Numeric current market price per unit.
            obs: Optional string for any additional notes or observations.
        """
        self.user_id = user_id
        self.symbol = symbol
        self.quantity = quantity
        self.purchase_price = purchase_price
        self.current_price = current_price
        self.current_value = self.calculate_current_value()
        self.variation = self.calculate_variation()
        self.purchase_datetime = purchase_datetime
        self.highest_price = highest_price
        self.should_be_sold = should_be_sold
        self.obs = obs

    def calculate_current_value(self) -> float:
        """Calculates the current total value of the asset."""
        return round(self.current_price * self.quantity, 2)

    def calculate_variation(self) -> float:
        """Calculates the variation of the asset."""
        return round(
            (((self.current_price - self.purchase_price) / self.current_price) * 100), 2
        )

    def update_asset(self, new_price: float) -> "Asset":
        """Updates the current price, the highest price if the new price is higher and recalculates variation."""
        self.current_price = new_price
        self.variation = self.calculate_variation()
        if self.current_price > self.highest_price:
            self.highest_price = self.current_price
        self.current_value = self.calculate_current_value()
        self.should_be_sold = self.update_should_asset_be_sold()
        return self

    def update_should_asset_be_sold(self) -> bool:
        if self.variation <= (-GlobalSettings.SELLING_UNDER_PURCHASE_PERCENTAGE):
            return True
        if self.current_price <= self.highest_price * (
            1 - GlobalSettings.SELLING_UNDER_HIGHEST_PERCENTAGE / 100
        ):
            return True
        if self.variation >= GlobalSettings.SELLING_ABOVE_PURCHASE_PERCENTAGE:
            return True
        return False

    def calculate_profit_loss(self) -> float:
        """Calculates the profit or loss of the asset."""
        total_purchase_value = self.purchase_price * self.quantity
        return self.current_value - total_purchase_value

    @classmethod
    def from_dict(cls, asset_dict: dict) -> "Asset":
        """TODO: Document method"""
        user_id = asset_dict["user_id"]
        symbol = asset_dict["symbol"]
        quantity = asset_dict["quantity"]
        purchase_price = asset_dict["purchase_price"]
        purchase_datetime = asset_dict["purchase_datetime"]
        highest_price = asset_dict["highest_price"]
        current_price = asset_dict["current_price"]
        should_be_sold = asset_dict["should_be_sold"]
        obs = asset_dict["obs"]
        return cls(
            user_id,
            symbol,
            quantity,
            purchase_price,
            purchase_datetime,
            highest_price,
            current_price,
            should_be_sold,
            obs,
        )

    def __str__(self) -> str:
        """Returns a string representation of the asset."""
        return (
            f"User ID: {self.user_id}\n"
            f"Symbol: {self.symbol}, Quantity: {self.quantity}\n"
            f"Purchase Price: {self.purchase_price:.2f}\n"
            f"Purchase Datetime: {self.purchase_datetime}\n"
            f"Highest Price: {self.highest_price:.2f}\n"
            f"Current Price: {self.current_price:.2f}\n"
            f"Variation: {self.variation:.2f}%\n"
            f"Should be Sold: {self.should_be_sold}\n"
        )

    def __in__(self):
        return True


'''

    @classmethod
    def from_series(cls, series: pd.Series) -> "Asset":
        """Creates an Asset object from a pandas Series."""
        symbol = series["symbol"]
        quantity = float(series["quantity"])
        purchase_price = float(series["purchase_price"])
        current_price = float(series["current_price"])
        purchase_datetime = datetime.datetime.strptime(
            series["purchase_datetime"], "%Y-%m-%d %H:%M:%S"
        )
        highest_price = float(series["highest_price"])
        obs = series["obs"]

        return cls(
            symbol,
            quantity,
            purchase_price,
            purchase_datetime,
            highest_price,
            current_price,
            obs,
        )


    @classmethod
    def _create_wallet_table(cls) -> None:
        """TODO: Document method"""
        table_name = "Wallets"
        sql = f"""
        CREATE TABLE IF NOT EXISTS {table_name} (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            user_id TEXT NOT NULL,
            balance REAL NOT NULL,
            FOREIGN KEY (user_id) REFERENCES Users(id)
        );
        """
        self.database_manager.execute_sql(sql, f"Error creating {table_name} table")

    def get_wallet_usdt_balance(self) -> float:
        """
        Retrieves the current USDT balance from the database.

        Returns:
            float: Current USDT balance.
        """
        table_name = "Wallets"
        sql = f"SELECT quantity FROM {table_name} WHERE symbol == ?"
        params = ("USDT",)
        balance = self._fetch_one(sql, params, "Error selecting USDT")
        return balance[0] if balance else 0

    def update_usdt_balance(self, value: float) -> None:
        """
        Updates the USDT balance in the database.

        Args:
            value (float): New USDT balance.
        """
        table_name = "Assets"
        sql = (
            f"UPDATE {table_name} SET quantity = ?, current_value = ? WHERE symbol = ?"
        )
        params = (value, value, "USDT")
        self._execute_sql(sql, f"Error updating USDT", params)

    def get_total_asset_value(self) -> float:
        """
        Obtains the sum of all 'current_value' entries in the 'Assets' table.

        Returns:
            float: Sum of all 'current_value' entries.
        """

        with sqlite3.connect(self.db_path) as conn:
            sql = "SELECT SUM(current_value) FROM Assets"
            result = conn.execute(sql).fetchone()
            return result[0] if result else 0

    def get_wallet_dataframe(self) -> pd.DataFrame:
        """
        Retrieves asset information from the database as a DataFrame.

        Returns:
            pd.DataFrame: DataFrame containing asset information.
        """
        table_name = "Assets"
        if not self.table_exists(table_name):
            self._create_assets_table()
        sql = f"SELECT * FROM {table_name}"
        return self._execute_sql(
            sql, f"Error selecting from {table_name}", table_name=table_name
        )

    def update_asset(self, asset: Asset) -> None:
        """
        Updates asset information in the database.

        Args:
            asset (Asset): Object containing updated asset data.
        """
        table_name = "Assets"
        sql = f"UPDATE {table_name} SET quantity = ?, current_value = ?, variation = ?, highest_price = ?, current_price = ? WHERE symbol = ?"
        params = (
            asset.quantity,
            asset.current_value,
            asset.variation,
            asset.highest_price,
            asset.current_price,
            asset.symbol,
        )
        self._execute_sql(
            sql, f"Error inserting price for {asset.symbol}", params)

    def delete_from_assets(self, symbol: str) -> None:
        """
        Deletes asset information for a specific symbol from the database.

        Args:
            symbol (str): Symbol of the asset to delete.
        """
        table_name = "Assets"
        sql = f"DELETE FROM {table_name} WHERE symbol == ?"
        params = (symbol,)
        self._execute_sql(
            sql, f"Error deleting price for {table_name}", params)
        self.drop_table(symbol)

    def has_balance(self, operation_value: float) -> tuple[bool, float]:
        """
        Check if there is enough balance to perform an operation.
        Args:
            operation_value (float): The value of the operation.
        Returns:
            tuple[bool, float]: True if there is enough balance, False otherwise. Balance.
        """
        balance = self.data_manager.get_database_usdt_balance()
        has_balance = balance >= operation_value
        return has_balance, balance

    def update_balance(self, value: float) -> float:
        """
        Update the balance in the database.
        Args:
            value (float): The value to be added or subtracted from the balance.
        Returns:
            float: The new balance after the update.
        """
        balance = self.data_manager.get_database_usdt_balance()
        new_balance = round((balance + value), 2)
        self.data_manager.update_usdt_balance(new_balance)
        return new_balance

    def insert_usdt(self, value: float) -> None:
        """
        Inserts USDT value into the database.
        Args:
            value (float): The USDT value to be inserted.
        """
        current_datetime = DateTimeUtils.get_datetime()
        self.data_manager.insert_usdt(value, current_datetime)

    @staticmethod
    def buy_asset(
        row: pd.Series,
        operation_value: float,
        database_manager: DatabaseManager,
    ) -> None:
        """
        Execute a buy operation for a given asset.

        Args:
            row (Series): Series containing asset data.
            operation_value (float): The value of the operation.
            data_manager (DataManager): DataManager object.
        """
        current_datetime = DateTimeUtils.get_datetime()
        symbol = row["symbol"]
        new_balance = balance_manager.update_balance(-operation_value)
        quantity = operation_value / row["current_price"]
        asset = Asset(
            symbol=symbol,
            quantity=quantity,
            purchase_price=row["current_price"],
            purchase_datetime=current_datetime,
            highest_price=row["current_price"],
            current_price=row["current_price"],
            obs=f"{row['variation']:.2f}% - {row['interval']}",
        )
        # Simulates binance transaction
        time.sleep(3)
        database_manager.insert_purchase(asset)
        final_balance = database_manager.get_assets_dataframe()["current_value"].sum()
        transaction_data = TransactionData(
            date=current_datetime.date(),
            time=current_datetime.strftime("%H:%M:%S"),
            order_type="Compra",
            quantity=asset.quantity,
            coin=asset.symbol,
            USDT_quantity=operation_value,
            purchase_price=asset.current_price,
            sell_price=None,
            profit_loss=None,
            variation=f'{row["variation"]:.2f}',
            interval=row["interval"],
            trading_fee=0.00,
            USDT_balance=new_balance,
            final_balance=final_balance,
        )
        TransactionManager.log_transaction_data(transaction_data)

    @staticmethod
    def sell_asset(
        asset: Asset,
        database_manager: DatabaseManager,
    ) -> None:
        """
        Execute a sell operation for a given asset.

        Args:
            asset (Asset): The asset to be sold.
            data_manager (DataManager): DataManager object.
        """
        current_datetime = DateTimeUtils.get_datetime()
        # Simulates binance transaction
        time.sleep(3)
        database_manager.delete_from_assets(asset.symbol)
        profit_loss = asset.calculate_profit_loss()
        new_balance = balance_manager.update_balance(asset.current_value)
        interval = DateTimeUtils.timedelta_to_string(
            current_datetime - asset.purchase_datetime
        )
        final_balance = database_manager.get_assets_dataframe()["current_value"].sum()
        transaction_data = TransactionData(
            date=current_datetime.date(),
            time=current_datetime.strftime("%H:%M:%S"),
            order_type="Venda",
            quantity=asset.quantity,
            coin=asset.symbol,
            USDT_quantity=asset.current_value,
            purchase_price=asset.purchase_price,
            sell_price=asset.current_price,
            profit_loss=profit_loss,
            variation=f"{asset.variation:.2f}",
            interval=interval,
            trading_fee=0.00,
            USDT_balance=new_balance,
            final_balance=final_balance,
        )
        TransactionManager.log_transaction_data(transaction_data)

    @staticmethod
    def log_transaction_data(transaction_data: TransactionData) -> None:
        """
        Logs a transaction data into a CSV file.

        Args:
            transaction_data (TransactionData): The transaction data to be logged.
        """
        current_datetime = DateTimeUtils.get_datetime()
        current_date = current_datetime.date()
        file_name = f"logs/log_execution_{current_date}.csv"

        fieldnames = [
            "Data",
            "Hora",
            "Tipo de ordem",
            "Quantidade",
            "Moeda",
            "Quantidade USDT",
            "Preco de compra",
            "Preco de venda",
            "Lucro/prejuizo",
            "Variacao",
            "Intervalo",
            "Taxa de negociacao",
            "Saldo USDT",
            "Saldo final",
        ]
        try:
            with open(file_name, "r") as file:
                csv.reader(file)
        except FileNotFoundError:
            with open(file_name, "w", newline="") as file:
                writer = csv.DictWriter(file, fieldnames=fieldnames)
                writer.writeheader()

        with open(file_name, "a", newline="") as file:
            writer = csv.DictWriter(file, fieldnames=fieldnames)
            row = {
                "Data": transaction_data.date,
                "Hora": transaction_data.time,
                "Tipo de ordem": transaction_data.order_type,
                "Quantidade": transaction_data.quantity,
                "Moeda": transaction_data.coin,
                "Quantidade USDT": transaction_data.USDT_quantity,
                "Preco de compra": transaction_data.purchase_price,
                "Preco de venda": transaction_data.sell_price,
                "Lucro/prejuizo": transaction_data.profit_loss,
                "Variacao": transaction_data.variation,
                "Intervalo": transaction_data.interval,
                "Taxa de negociacao": transaction_data.trading_fee,
                "Saldo USDT": transaction_data.USDT_balance,
                "Saldo final": transaction_data.final_balance,
            }
            writer.writerow(row)
            logging.basicConfig(level=logging.INFO)
            logger = logging.getLogger("altacrypto")
            logger.info(transaction_data)

    @staticmethod
    def attempt_purchase(
        current_datetime: datetime,
        balance: float,
    ) -> None:
        """TODO: document method."""
        transaction_data = TransactionData(
            date=current_datetime.date(),
            time=current_datetime.strftime("%H:%M:%S"),
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
            final_balance=balance,
        )
        TransactionManager.log_transaction_data(transaction_data)

    def __str__(self) -> str:
        """Returns a string representation of the wallet."""
        assets_info = "\n".join(str(asset) for asset in self.assets)
        return f"Wallet Balance: {self.balance:.2f}\nAssets:\n{assets_info}"


'''

