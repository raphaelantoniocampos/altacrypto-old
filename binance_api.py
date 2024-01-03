from binance.client import Client
import pandas as pd

class BinanceAPI:
    def __init__(self, api_key, api_secret, testnet=False):
        self.client = Client(api_key, api_secret, testnet)
    
    def fetch_usdt_pairs(self):
        """
        Retrieves and filters USDT pairs from Binance.
        """
        try:
            tickers = self.client.get_all_tickers()
            coin_prices = pd.DataFrame(tickers)
            return coin_prices[coin_prices["symbol"].str.endswith("USDT")]
        except Exception as e:
            print(f"Error fetching USDT pairs from Binance: {e}")
            return pd.DataFrame()
