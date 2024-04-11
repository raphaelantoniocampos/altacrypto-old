import pandas as pd
import requests

from utils.user_settings import UserSettings


class BinanceManager:
    """
    Provides methods to interact with the Binance API for retrieving data and checking system status.

    Attributes:
        api_key (str): API key for accessing the Binance API.
        api_secret (str): API secret for accessing the Binance API.
        base_url (str): Base URL of the Binance API.
    """

    def __init__(self, user_settings: UserSettings, api_key: str, api_secret: str):
        """
        Initializes a BinanceAPI object with API key and secret.

        Args:
            api_key (str): API key for accessing the Binance API.
            api_secret (str): API secret for accessing the Binance API.
        """
        self.user_settings = user_settings
        self.api_key = api_key
        self.api_secret = api_secret
        self.base_url = 'https://api.binance.com'

    def query_binance_status(self) -> bool:
        """
        Queries the status of the Binance system.

        Returns:
            bool: True if the system is operational, False otherwise.
        """
        try:
            endpoint = "/sapi/v1/system/status"
            response = self._make_request(endpoint)
            status = response.json()

            if status['status'] == 0:
                return True
            else:
                return False

        except requests.RequestException as e:
            self.user_settings.logger.info(f"Error connecting to binance API: {e}")
            return False

    def fetch_usdt_pairs(self) -> pd.DataFrame:
        """
        Fetches USDT pairs from Binance.

        Returns:
            pd.DataFrame: DataFrame containing USDT pairs.
        """
        try:
            endpoint = "/api/v3/ticker/price"
            response = self._make_request(endpoint)
            tickers = response.json()
            coin_prices = pd.DataFrame(tickers)
            return coin_prices[coin_prices["symbol"].str.endswith("USDT")]

        except Exception as e:
            self.user_settings.logger.info(
                f"Error fetching USDT pairs from Binance: {e}"
            )
            return pd.DataFrame()

    def _make_request(self, endpoint: str, params: dict | None = None) -> requests.Response:
        """
        Makes a request to the Binance API.

        Args:
            endpoint (str): The API endpoint to query.
            params (dict, optional): Additional parameters for the request.

        Returns:
            requests.Response: Response object from the API request.

        Raises:
            Exception: If the request fails with a non-200 status code.
        """
        url = f"{self.base_url}{endpoint}"
        headers = {"X-MBX-APIKEY": self.api_key}
        response = requests.get(url, params=params, headers=headers)

        if response.status_code != 200:
            raise Exception(
                f"Request failed with status code {response.status_code}: {response.text}")

        return response


