import bson
from models.asset import Asset


class Order:
    """Represents a generic order."""

    def __init__(self, user_id: bson.objectid.ObjectId | None, side: str, interval):
        """
        Initializes an Order object.

        Args:
            user_id (bson.objectid.ObjectId | None): The ID of the user placing the order.
            side (str): The side of the order (e.g., "BUY" or "SELL").
            interval: The interval of the order.
        """
        self.user_id = user_id
        self.side = side.upper()
        self.interval = interval


class SellOrder(Order):
    """Represents a sell order."""

    def __init__(self, side: str, interval, asset: Asset):
        """
        Initializes a SellOrder object.

        Args:
            side (str): The side of the order (e.g., "BUY" or "SELL").
            interval: The interval of the order.
            asset (Asset): The asset to be sold.
        """
        super().__init__(asset.user_id, side, interval)
        self.asset = asset

    def __str__(self) -> str:
        """
        Returns a string representation of the SellOrder.
        """
        return (
            f"User ID: {self.asset.user_id}\n"
            f"Side: {self.side}\n"
            f"Symbol: {self.asset.symbol}\n"
            f"Highest Price: {self.asset.highest_price}\n"
            f"Current Price: {self.asset.current_price}\n"
            f"Variation: {self.asset.variation}%\n"
            f"Purchase Datetime {self.asset.purchase_datetime}\n"
        )


class BuyOrder(Order):
    """Represents a buy order."""

    def __init__(
        self, side: str, interval, symbol: str, variation: float, current_price: float
    ):
        """
        Initializes a BuyOrder object.

        Args:
            side (str): The side of the order (e.g., "BUY" or "SELL").
            interval: The interval of the order.
            symbol (str): The symbol of the asset to be bought.
            variation (float): The variation for the buy order.
            current_price (float): The current price of the asset.
        """
        super().__init__(None, side, interval)
        self.symbol = symbol
        self.variation = round(variation, 2)
        self.current_price = current_price

    def __str__(self) -> str:
        """Returns a string representation of the BuyOrder."""
        return (
            f"User ID: None\n"
            f"Side: {self.side}\n"
            f"Symbol: {self.symbol}\n"
            f"Interval: {self.interval}\n"
            f"Variation: {self.variation}%\n"
        )
