import bson
from models.asset import Asset


class Order:
    """TODO: Document class"""

    def __init__(
        self,
        user_id: bson.objectid.ObjectId | str,
        order_type: str,
        symbol: str,
    ):
        """TODO: Document method"""
        self.user_id = user_id
        self.order_type = order_type.lower()
        self.symbol = symbol

    def __str__(self) -> str:
        return f"{self.user_id} - {self.order_type} - {self.symbol}"
