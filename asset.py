class Asset:
    def __init__(self, symbol, purchase_datetime, purchase_price, highest_price, current_price, variation):
        self.symbol = symbol
        self.purchase_datetime = purchase_datetime
        self.purchase_price = purchase_price
        self.highest_price = highest_price
        self.current_price = current_price
        self.variation = variation
    
    @classmethod
    def from_series(cls, series):
        symbol = series['symbol']
        purchase_datetime = series['purchase_datetime']
        purchase_price = series['purchase_price']
        highest_price = series['highest_price']
        current_price = series['current_price']
        variation = series['variation']

        return cls(symbol, purchase_datetime, purchase_price, highest_price, current_price, variation)

    def __str__(self) -> str:
        return f"""
        Symbol: {self.symbol}\n
        Purchase Datetime: {self.purchase_datetime}\n
        Purchase Price: {self.purchase_price}\n
        Highest Price: {self.highest_price}\n
        Current Price: {self.current_price}\n
        Variation: {self.variation}"""
