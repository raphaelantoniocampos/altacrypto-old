from datetime import datetime

class PriceSnapshot:
    """Represents a snapshot of a historical asset price."""

    def __init__(self, symbol, timestamp, price):
        """
        Initializes a PriceSnapshot object with data validation.

        Args:
            symbol: String representing the asset symbol.
            timestamp: Integer timestamp of the price.
            price: Numeric price per unit.

        """

        # Initialize attributes
        self.symbol = symbol.upper()
        self.timestamp = timestamp
        self.price = price
        self.datetime = datetime.fromtimestamp(timestamp)

    def __str__(self) -> str:
        """Returns a string representation of the price snapshot."""
        return f"Symbol: {self.symbol}, Timestamp: {self.timestamp}, " \
               f"Price: {self.price:.2f}, Datetime: {self.datetime}"