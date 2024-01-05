import requests
import pandas as pd

class BinanceAPI:
    def __init__(self, api_key, api_secret):
        self.api_key = api_key
        self.api_secret = api_secret
        self.base_url = 'https://api.binance.us'

    
    def fetch_usdt_pairs(self):
        try:
            endpoint = "/api/v3/ticker/price"
            response = self._make_request(endpoint)
            tickers = response.json()
            coin_prices = pd.DataFrame(tickers)
            return coin_prices[coin_prices["symbol"].str.endswith("USDT")]

        except Exception as e:
            print(f"Error fetching USDT pairs from Binance: {e}")
            return pd.DataFrame()

    def _make_request(self, endpoint, params=None):
        url = f"{self.base_url}{endpoint}"
        headers = {"X-MBX-APIKEY": self.api_key}
        response = requests.get(url, params=params, headers=headers)
        
        if response.status_code != 200:
            raise Exception(f"Request failed with status code {response.status_code}: {response.text}")
        
        return response
   
    