import pandas as pd
from datetime import datetime


class CryptoSnapshot:
    """Represents a snapshot of cryptocurrency data at a specific datetime."""

    def __init__(self, symbol: str, datetime: datetime, price: float):
        """
        Initializes a CryptoSnapshot object.

        Args:
            symbol (str): The symbol of the cryptocurrency.
            datetime (datetime): The datetime of the snapshot.
            price (float): The price of the cryptocurrency at the snapshot.
        """
        self.symbol = symbol.upper()
        self.datetime = datetime
        self.price = price

    def __str__(self) -> str:
        """Returns a string representation of the Crypto Snapshot."""
        return f"Symbol: {self.symbol}\nDatetime: {self.datetime}\nPrice: {self.price}"

    @classmethod
    def from_series(cls, series: pd.Series, datetime: datetime) -> "CryptoSnapshot":
        """
        Creates a CryptoSnapshot object from a Pandas Series.

        Args:
            series (pd.Series): The Pandas Series containing snapshot data.
            datetime (datetime): The datetime of the snapshot.

        Returns:
            CryptoSnapshot: The CryptoSnapshot object created from the Series.
        """
        symbol = series["symbol"]
        datetime = datetime
        price = float(series["price"])
        return cls(symbol, datetime, price)

    @classmethod
    def from_dict(cls, crypto_dict: dict) -> "CryptoSnapshot":
        """
        Creates a CryptoSnapshot object from a dictionary.

        Args:
            crypto_dict (dict): The dictionary containing snapshot data.

        Returns:
            CryptoSnapshot: The CryptoSnapshot object created from the dictionary.
        """
        symbol = crypto_dict["symbol"]
        datetime = crypto_dict["datetime"]
        price = crypto_dict["price"]
        return cls(symbol, datetime, price)

