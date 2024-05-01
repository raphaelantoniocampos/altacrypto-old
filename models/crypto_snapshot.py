import pandas as pd
from datetime import datetime


class CryptoSnapshot:
    """TODO: Document class"""

    def __init__(self, symbol: str, datetime: datetime, price: float):
        """TODO: Document method"""
        self.symbol = symbol.upper()
        self.datetime = datetime
        self.price = price

    def __str__(self) -> str:
        """Returns a string representation of the Crypto Snapshot."""
        return f"Symbol: {self.symbol}\nDatetime: {self.datetime}\nPrice: {self.price}"

    @classmethod
    def from_series(cls, series: pd.Series, datetime: datetime) -> "CryptoSnapshot":
        """TODO: Document method"""
        symbol = series["symbol"]
        datetime = datetime
        price = float(series["price"])
        return cls(symbol, datetime, price)

    @classmethod
    def from_dict(cls, crypto_dict: dict) -> "CryptoSnapshot":
        """TODO: Document method"""
        symbol = crypto_dict["symbol"]
        datetime = crypto_dict["datetime"]
        price = crypto_dict["price"]
        return cls(symbol, datetime, price)
