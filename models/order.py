import bson

from models.asset import Asset


class Order:
    """TODO: Document class"""

    def __init__(self, user_id: bson.objectid.ObjectId | None, side: str, interval):
        """TODO: Document method"""
        self.user_id = user_id
        self.side = side.upper()
        self.interval = interval


class SellOrder(Order):
    """TODO: Document class"""

    def __init__(self, side: str, interval, asset: Asset):
        """TODO: Document method"""
        super().__init__(asset.user_id, side, interval)
        self.asset = asset

    def __str__(self) -> str:
        return (
            f"User ID: {self.asset.user_id}\n"
            f"Side: {self.side}\n"
            f"Symbol: {self.asset.symbol}\n"
            f"Interval: {self.interval}\n"
            f"Variation: {self.asset.variation}\n"
        )


class BuyOrder(Order):
    """TODO: Document class"""

    def __init__(
        self, side: str, interval, symbol: str, variation: float, current_price: float
    ):
        """TODO: Document method"""
        super().__init__(None, side, interval)
        self.symbol = symbol
        self.variation = variation
        self.current_price = current_price

    def __str__(self) -> str:
        return (
            f"User ID: None\n"
            f"Side: {self.side}\n"
            f"Symbol: {self.symbol}\n"
            f"Interval: {self.interval}\n"
            f"Variation: {self.variation}\n"
        )

