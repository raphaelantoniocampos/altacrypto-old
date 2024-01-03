class PriceSnapshot:
    def __init__(self, symbol, time, date, price):
        self.symbol = symbol
        self.time = time
        self.date = date
        self.price = price

    def __str__(self) -> str:
        return f'Symbol: {self.symbol}\nTime: {self.time}\nDate: {self.date}\nPrice: {self.price}'

