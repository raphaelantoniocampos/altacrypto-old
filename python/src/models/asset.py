import datetime
import bson
from models.user import UserSettings


class Asset:
    """Represents an investment asset with details like symbol, quantity, prices, and variation."""

    def __init__(
        self,
        user_id: bson.objectid.ObjectId | None,
        symbol: str,
        quantity: float,
        purchase_price: float,
        purchase_datetime: datetime.datetime,
        highest_price: float,
        current_price: float,
        _id: bson.objectid.ObjectId | None = None,
        should_be_sold: bool = False,
        sold: datetime.datetime | None = None,
    ):
        """
        Initializes an Asset object.

        Args:
            user_id (bson.objectid.ObjectId | None): The ID of the user who owns the asset.
            symbol (str): The symbol of the asset.
            quantity (float): The quantity of the asset.
            purchase_price (float): The purchase price of the asset.
            purchase_datetime (datetime.datetime): The datetime of the asset purchase.
            highest_price (float): The highest price of the asset.
            current_price (float): The current price of the asset.
            _id (bson.objectid.ObjectId | None, optional): The ID of the asset. Defaults to None.
            should_be_sold (bool, optional): Indicates if the asset should be sold. Defaults to False.
            sold (datetime.datetime | None, optional): The datetime when the asset was sold. Defaults to None.
        """
        self._id = _id
        self.user_id = user_id
        self.symbol = symbol
        self.quantity = quantity
        self.purchase_price = purchase_price
        self.current_price = current_price
        self.current_value = self.calculate_current_value()
        self.variation = self.calculate_variation()
        self.purchase_datetime = purchase_datetime
        self.highest_price = highest_price
        self.should_be_sold = should_be_sold
        self.sold = sold

    def calculate_current_value(self) -> float:
        """Calculates the current total value of the asset."""
        return round(self.current_price * self.quantity, 2)

    def calculate_variation(self) -> float:
        """Calculates the variation of the asset."""
        return round(
            (((self.current_price - self.purchase_price) / self.current_price) * 100), 2
        )

    def update_asset(self, new_price: float, user_settings: UserSettings) -> "Asset":
        """
        Updates the asset with a new price.

        Args:
            new_price (float): The new price of the asset.

        Returns:
            Asset: The updated asset object.
        """
        self.current_price = new_price
        self.variation = self.calculate_variation()
        if self.current_price > self.highest_price:
            self.highest_price = self.current_price
        self.current_value = self.calculate_current_value()
        self.should_be_sold = self.update_should_asset_be_sold(user_settings)
        return self

    def update_should_asset_be_sold(self, user_settings: UserSettings) -> bool:
        """Determines if the asset should be sold based on predefined criteria."""
        if self.variation <= (-user_settings.selling_under_purchase_percentage):
            return True
        if self.current_price <= self.highest_price * (
            1 - user_settings.selling_under_highest_percentage / 100
        ):
            return True
        if self.variation >= user_settings.selling_above_purchase_percentage:
            return True
        return False

    def calculate_profit_loss(self) -> float:
        """Calculates the profit or loss of the asset."""
        total_purchase_value = self.purchase_price * self.quantity
        return self.current_value - total_purchase_value

    @classmethod
    def from_dict(cls, asset_dict: dict) -> "Asset":
        """
        Creates an Asset object from a dictionary.

        Args:
            asset_dict (dict): The dictionary containing asset data.

        Returns:
            Asset: The Asset object created from the dictionary.
        """
        user_id = asset_dict["user_id"]
        symbol = asset_dict["symbol"]
        quantity = asset_dict["quantity"]
        purchase_price = asset_dict["purchase_price"]
        purchase_datetime = asset_dict["purchase_datetime"]
        highest_price = asset_dict["highest_price"]
        current_price = asset_dict["current_price"]
        should_be_sold = asset_dict["should_be_sold"]
        sold = asset_dict["sold"]
        return cls(
            user_id,
            symbol,
            quantity,
            purchase_price,
            purchase_datetime,
            highest_price,
            current_price,
            should_be_sold,
            sold,
        )

    def __str__(self) -> str:
        """Returns a string representation of the asset."""
        return (
            f"User ID: {self.user_id}\n"
            f"Symbol: {self.symbol}, Quantity: {self.quantity}\n"
            f"Purchase Price: {self.purchase_price:.2f}\n"
            f"Purchase Datetime: {self.purchase_datetime}\n"
            f"Highest Price: {self.highest_price:.2f}\n"
            f"Current Price: {self.current_price:.2f}\n"
            f"Variation: {self.variation:.2f}%\n"
            f"Should be Sold: {self.should_be_sold}\n"
        )
