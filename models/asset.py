import datetime

class Asset:
    """Represents an investment asset with details like symbol, quantity, prices, and performance."""
    def __init__(self, symbol, quantity, purchase_price, purchase_datetime, highest_price, current_price, obs = None):
        """
        Initializes an Asset object with data validation.

        Args:
            symbol: String representing the asset symbol.
            quantity: Integer number of units owned.
            purchase_price: Numeric purchase price per unit.
            purchase_datetime: Datetime object of the purchase date.
            highest_price: Numeric highest price reached since purchase.
            current_price: Numeric current market price per unit.
            variation: Numeric percentage change since purchase (can be negative).
            obs: Optional string for any additional notes or observations.

        Raises:
            ValueError: If data is invalid (e.g., negative quantity, price inconsistencies).
        """
        # Validate data types and values
        if not isinstance(symbol, str):
            raise ValueError("Symbol must be a string.")
        if not isinstance(quantity, (float, int)):
            raise ValueError("Quantity must be numeric.")
        if quantity < 0:
            raise ValueError("Quantity cannot be negative.")
        if not isinstance(purchase_price, (float, int)):
            raise ValueError("Purchase price must be numeric.")
        if not isinstance(purchase_datetime, (datetime.datetime, str)):
            raise ValueError("Purchase date must be a datetime object or string.")
        if not isinstance(highest_price, (float, int)):
            raise ValueError("Highest price must be numeric.")
        if not isinstance(current_price, (float, int)):
            raise ValueError("Current price must be numeric.")
        if purchase_price > highest_price:
            raise ValueError("Purchase price cannot be higher than highest price.")

        # Initialize attributes
        self.symbol = symbol.upper()
        self.quantity = quantity
        self.purchase_price = purchase_price
        self.current_price = current_price
        self.current_value = self.calculate_current_value()
        self.variation = self.calculate_variation()

        self.purchase_datetime = purchase_datetime
        self.highest_price = highest_price
        
        
        self.obs = obs
    
    def calculate_current_value(self):
        """Calculates the current total value of the asset."""
        return round(self.current_price * self.quantity, 2)
    
    def calculate_variation(self):
        """Calculates the variation of the asset."""
        return round((((self.current_price - self.purchase_price) / self.current_price) * 100), 2)

    def update_asset(self, new_price):
        """Updates the current price, the highest price if the new price is higher and recalculates variation."""
        self.current_price = new_price
        self.variation = self.calculate_variation()
        if self.current_price > self.highest_price:
            self.highest_price = self.current_price
        self.current_value = self.calculate_current_value()

    def calculate_profit_loss(self):
        total_purchase_value = self.purchase_price * self.quantity
        return self.current_value - total_purchase_value

    def __str__(self):
        """Returns a string representation of the asset."""
        return f"Symbol: {self.symbol}, Quantity: {self.quantity}, " \
               f"Purchase Price: {self.purchase_price:.2f}, " \
               f"Current Price: {self.current_price:.2f}, " \
               f"Variation: {self.variation:.2f}%, " \
               f"Highest Price: {self.highest_price:.2f}"
    
    @classmethod
    def from_series(cls, series):
        symbol = series['symbol']
        quantity = series['quantity']
        purchase_price = series['purchase_price']
        current_price = series['current_price']
        purchase_datetime = series['purchase_datetime']
        highest_price = series['highest_price']
        obs = series['obs'] 

        return cls(symbol, quantity, purchase_price, purchase_datetime, highest_price, current_price, obs)
