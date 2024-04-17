class Wallet:
    """Represents a user's wallet."""

    def __init__(self, balance: float):
        """
        Initializes a Wallet object with a balance.

        Args:
            balance: Initial balance of the wallet.
        """
        self.balance = balance
        self.assets = []

    def add_asset(self, asset: "Asset") -> None:
        """Adds an asset to the wallet."""
        self.assets.append(asset)

    def __str__(self) -> str:
        """Returns a string representation of the wallet."""
        assets_info = "\n".join(str(asset) for asset in self.assets)
        return f"Wallet Balance: {self.balance:.2f}\nAssets:\n{assets_info}"

