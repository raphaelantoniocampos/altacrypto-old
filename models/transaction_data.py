class TransactionData:
    """Represents a transaction with its associated data."""

    def __init__(self, date, time, order_type, quantity, coin, USDT_quantity, purchase_price, sell_price, profit_loss, variation, interval, trading_fee, USDT_balance, final_balance):
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
        return f"""{self.date} - {self.time}
{self.coin} - {self.order_type}"""