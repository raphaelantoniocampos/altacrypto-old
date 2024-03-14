import csv
from utils.datetime_utils import DateTimeUtils
import utils.settings as settings
from models.asset import Asset
from models.transaction_data import TransactionData
import time
from datetime import datetime


class TransactionManager:

    @staticmethod
    def buy_asset(row, operation_value, balance_manager, data_manager):
        """
        Execute a buy operation for a given asset.

        Args:
            row (Series): Series containing asset data.
            operation_value (float): The value of the operation.
        """
        current_datetime = DateTimeUtils.get_datetime()
        symbol = row['symbol']
        new_balance = balance_manager.update_balance(-operation_value)
        quantity = operation_value / row['current_price']
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
        data_manager.insert_purchase(asset)
        final_balance = data_manager.get_assets_dataframe()['current_value'].sum()
        transaction_data = TransactionData(
            date=current_datetime.date(),
            time=current_datetime.strftime('%H:%M:%S'),
            order_type="Compra",
            quantity=asset.quantity,
            coin=asset.symbol,
            USDT_quantity=operation_value,
            purchase_price=asset.current_price,
            sell_price=None,
            profit_loss=None,
            variation=f'{row["variation"]:.2f}',
            interval=row['interval'],
            trading_fee=0.00,
            USDT_balance=new_balance,
            final_balance=final_balance
        )
        TransactionManager.log_transaction_data(transaction_data)

    @staticmethod
    def sell_asset(asset, balance_manager, data_manager):
        """
        Execute a sell operation for a given asset.

        Args:
            asset (Asset): The asset to be sold.
        """
        current_datetime = DateTimeUtils.get_datetime()
        # Simulates binance transaction
        time.sleep(3)
        data_manager.delete_from_assets(asset.symbol)
        profit_loss = asset.calculate_profit_loss()
        new_balance = balance_manager.update_balance(asset.current_value)
        interval = DateTimeUtils.timedelta_to_string(
            current_datetime
            - datetime.strptime(asset.purchase_datetime, "%Y-%m-%d %H:%M:%S")
        )
        final_balance = data_manager.get_assets_dataframe()["current_value"].sum()
        transaction_data = TransactionData(
            date=current_datetime.date(),
            time=current_datetime.strftime('%H:%M:%S'),
            order_type="Venda",
            quantity=asset.quantity,
            coin=asset.symbol,
            USDT_quantity=asset.current_value,
            purchase_price=asset.purchase_price,
            sell_price=asset.current_price,
            profit_loss=profit_loss,
            variation=f'{asset.variation:.2f}',
            interval=interval,
            trading_fee=0.00,
            USDT_balance=new_balance,
            final_balance=final_balance
        )
        TransactionManager.log_transaction_data(transaction_data)

    @staticmethod
    def log_transaction_data(transaction_data):
        """
        Logs a transaction data into a CSV file.

        Args:
            transaction_data (TransactionData): The transaction data to be logged.
        """
        current_datetime = DateTimeUtils.get_datetime()
        current_date = current_datetime.date()
        file_name = f'logs/log_execution_{current_date}.csv'

        fieldnames = ["Data", "Hora", "Tipo de ordem", "Quantidade", "Moeda", "Quantidade USDT", "Preco de compra", "Preco de venda", "Lucro/prejuizo", "Variacao", "Intervalo", "Taxa de negociacao", "Saldo USDT", "Saldo final"]
        try:
            with open(file_name, 'r') as file:
                csv.reader(file)
        except FileNotFoundError:
            with open(file_name, 'w', newline='') as file:
                writer = csv.DictWriter(file, fieldnames=fieldnames)
                writer.writeheader()

        with open(file_name, 'a', newline='') as file:
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
                "Saldo final": transaction_data.final_balance
            }
            writer.writerow(row)
            settings.logger.info(transaction_data)

    @staticmethod
    def attempt_purchase(current_datetime, balance):
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

