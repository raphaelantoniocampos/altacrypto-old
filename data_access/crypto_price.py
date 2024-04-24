import pandas as pd

class CryptoPrice:
    """TODO: Document class"""

    def __init__(self, symbol: str, timestamp: int, price: float):
        """TODO: Document method"""
        self.symbol = symbol.upper()
        self.snapshot = {"timestamp": timestamp, "price": price}

    def __str__(self) -> str:
        """Returns a string representation of the price snapshot."""
        return f"Symbol: {self.symbol}\nSnapshot: {self.snapshot}"

    @classmethod
    def from_series(cls, series: pd.Series, current_timestamp: int) -> "CryptoPrice":
        """TODO: Document method"""
        symbol = series["symbol"]
        price = series["price"]
        return cls(symbol, current_timestamp, price)

