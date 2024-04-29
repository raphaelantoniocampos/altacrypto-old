import pandas as pd
from bson.timestamp import Timestamp


class CryptoSnapshot:
    """TODO: Document class"""

    def __init__(self, symbol: str, timestamp: Timestamp, price: float):
        """TODO: Document method"""
        self.symbol = symbol.upper()
        self.timestamp = timestamp
        self.price = price

    def __str__(self) -> str:
        """Returns a string representation of the Crypto Snapshot."""
        return (
            f"Symbol: {self.symbol}\nTimestamp: {self.timestamp}\nPrice: {self.price}"
        )

    @classmethod
    def from_series(cls, series: pd.Series, current_timestamp: int) -> "CryptoSnapshot":
        """TODO: Document method"""
        symbol = series["symbol"]
        timestamp = Timestamp(current_timestamp, 0)
        price = float(series["price"])
        return cls(symbol, timestamp, price)
