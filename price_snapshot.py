from datetime import datetime

class PriceSnapshot:
    def __init__(self, symbol, timestamp, price):
        self.symbol = symbol
        self.timestamp = timestamp
        self.price = price
        self.datetime = datetime.fromtimestamp(self.timestamp)
        

    def __str__(self) -> str:
        return f'Symbol: {self.symbol}\nTimestamp: {self.timestamp}\nPrice: {self.price}\nDatetime: {self.datetime}'

