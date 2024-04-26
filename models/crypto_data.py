import pandas as pd
from bson.timestamp import Timestamp


class CryptoData:
    """TODO: Document class"""

    def __init__(self, symbol: str, snapshots: list[dict] = []):
        """TODO: Document method"""
        self.symbol = symbol.upper()
        self.snapshots = snapshots

    def __str__(self) -> str:
        """Returns a string representation of the crypto data."""
        return f"Symbol: {self.symbol}\nSnapshots: {self.snapshots}"

    @classmethod
    def from_series(cls, series: pd.Series, current_timestamp: int) -> "CryptoData":
        """TODO: Document method"""
        symbol = series["symbol"]
        price = float(series["price"])
        snapshots = [{"timestamp": Timestamp(current_timestamp, 0), "price": price}]
        return cls(symbol, snapshots)

