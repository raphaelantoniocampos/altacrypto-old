import csv
import logging
from models.transaction_data import TransactionData
from utils.datetime_utils import DateTimeUtils


class LoggingManager:
    logging.basicConfig(level=logging.INFO)
    logger = logging.getLogger("altacrypto")

    @staticmethod
    def log_asset_transaction(transaction_data: TransactionData) -> None:
        """
        Logs a transaction data into a CSV file.

        Args:
            transaction_data (TransactionData): The transaction data to be logged.
        """
        current_datetime = DateTimeUtils.get_datetime()
        current_date = current_datetime.date()
        file_name = f'../../logs/log_execution_{current_date}.csv'

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
            LoggingManager.logger.info(transaction_data)

