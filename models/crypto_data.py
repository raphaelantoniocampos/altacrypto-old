import pandas as pd
from typing import List
from bson.timestamp import Timestamp
from models.snapshot_dict import SnapshotDict


class CryptoData:
    """TODO: Document class"""

    def __init__(self, symbol: str, snapshots: List[SnapshotDict] = []):
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
        snapshot_dict: SnapshotDict = {
            "timestamp": Timestamp(current_timestamp, 0),
            "price": price,
        }
        snapshots = []
        snapshots.append(snapshot_dict)
        return cls(symbol, snapshots)
