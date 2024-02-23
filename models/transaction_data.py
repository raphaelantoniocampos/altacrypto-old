class TransactionData:
    """
    Represents a transaction with its associated data.

    Attributes:
        date (str or datetime.date): Date of the transaction.
        time (str): Time of the transaction.
        order_type (str): Type of order (e.g., "Compra" for buy, "Venda" for sell).
        quantity (int or float): Quantity of the asset involved in the transaction.
        coin (str): Symbol of the asset involved in the transaction.
        USDT_quantity (int or float): Quantity of USDT (Tether) involved in the transaction.
        purchase_price (int or float): Price per unit at the time of purchase.
        sell_price (int or float): Price per unit at the time of sale (if applicable).
        profit_loss (int or float): Profit or loss generated by the transaction.
        variation (int or float): Variation percentage associated with the transaction.
        interval (str): Interval associated with the transaction (e.g., "1 hour", "24 hours").
        trading_fee (int or float): Fee associated with the transaction.
        USDT_balance (int or float): Balance of USDT (Tether) after the transaction.
        final_balance (int or float): Final balance after the transaction.
    """

    def __init__(self, date, time, order_type, quantity, coin, USDT_quantity, purchase_price, sell_price, profit_loss, variation, interval, trading_fee, USDT_balance, final_balance):
        """
        Initializes a TransactionData object.

        Args:
            date (str or datetime.date): Date of the transaction.
            time (str): Time of the transaction.
            order_type (str): Type of order (e.g., "Compra" for buy, "Venda" for sell).
            quantity (int or float): Quantity of the asset involved in the transaction.
            coin (str): Symbol of the asset involved in the transaction.
            USDT_quantity (int or float): Quantity of USDT (Tether) involved in the transaction.
            purchase_price (int or float): Price per unit at the time of purchase.
            sell_price (int or float): Price per unit at the time of sale (if applicable).
            profit_loss (int or float): Profit or loss generated by the transaction.
            variation (int or float): Variation percentage associated with the transaction.
            interval (str): Interval associated with the transaction (e.g., "1 hour", "24 hours").
            trading_fee (int or float): Fee associated with the transaction.
            USDT_balance (int or float): Balance of USDT (Tether) after the transaction.
            final_balance (int or float): Final balance after the transaction.
        """
        self.date = date
        self.time = time
        self.order_type = order_type
        self.quantity = quantity
        self.coin = coin
        self.USDT_quantity = USDT_quantity
        self.purchase_price = purchase_price
        self.sell_price = sell_price
        self.profit_loss = profit_loss
        self.variation = variation
        self.interval = interval
        self.trading_fee = trading_fee
        self.USDT_balance = USDT_balance
        self.final_balance = final_balance

    def __str__(self):
        """
        Returns a string representation of the transaction.
        """
        return f"""{self.date} - {self.time}
{self.coin} - {self.order_type}"""