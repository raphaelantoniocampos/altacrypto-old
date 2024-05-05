import bson


class Order:
    """TODO: Document class"""

    def __init__(
        self,
        user_id: bson.objectid.ObjectId | str,
        side: str,
        symbol: str,
        order_info: dict,
    ):
        """TODO: Document method"""
        self.user_id = user_id
        self.side = side.upper()
        self.symbol = symbol
        self.order_info = order_info

    def __str__(self) -> str:
        return f"{self.user_id} - {self.order_type} - {self.symbol}\nVariation: {self.order_info['variation']} - Interval Time: {self.order_info['interval']}"
