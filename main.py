import os
import time

import pandas as pd
from dotenv import load_dotenv
from binance.client import Client
load_dotenv()

api_key = os.environ.get('BINANCE_API_KEY_TEST')
api_secret = os.environ.get('BINANCE_API_SECRET_TEST')

client = Client(api_key, api_secret, testnet=True)




while True:
    tickers = client.get_all_tickers()

    df = pd.DataFrame(tickers)
    current_pairs = df[df['symbol'].str.endswith("USDT")]

    time.sleep(30)
    previous_pairs = current_pairs

    tickers = client.get_all_tickers()
    df = pd.DataFrame(tickers)
    current_pairs = df[df['symbol'].str.endswith("USDT")]

    for index, row in current_pairs.iterrows():
        previous_pair = previous_pairs.loc[index]
        if previous_pair['symbol'] == row['symbol']:
            previous_price = float(previous_pair['price'])
            current_price = float(row['price'])
            variation = ((current_price - previous_price) / previous_price) * 100
            print(f"pair {row['symbol']} - variation {variation}")








'''
Matheus Code

# Configurações da API da Binance
api_key = '8ecf6810f960e613064fd8fe43d70194bf64f1796892356971a83c0b6a606469'
api_secret = '59ca242bf97f3c5c1908a9992d8e68b864092b84f82d5f80751a065ac430b169'

# URL da API da Binance
base_url = 'https://testnet.binance.vision/api'

# Função para obter o saldo atual em USDT
def get_usdt_balance():
    endpoint = 'account'
    params = {'timestamp': int(time.time()) * 1000}
    headers = {'X-MBX-APIKEY': api_key}
    response = requests.get(base_url + endpoint, params=params, headers=headers)
    data = response.json()
    for asset in data['balances']:
        if asset['asset'] == 'USDT':
            return float(asset['free'])
    return 0.0

# Função para obter o preço atual de um par de criptomoedas
def get_crypto_price(symbol):
    endpoint = 'ticker/price'
    response = requests.get(base_url + endpoint, params={'symbol': symbol})
    data = response.json()
    return float(data['price'])

# Função para realizar uma compra
def buy(symbol, quantity, usdt_balance):
    if usdt_balance > 0:
        endpoint = 'order'
        params = {
            'symbol': symbol,
            'side': 'BUY',
            'type': 'MARKET',
            'quantity': quantity,
        }
        headers = {'X-MBX-APIKEY': api_key}
        response = requests.post(base_url + endpoint, params=params, headers=headers)
        data = response.json()
        return data
    return None

if __name__ == "__main":
    while True:
        try:
            # Obtém o saldo em USDT
            usdt_balance = get_usdt_balance()

            # Obtém uma lista de todos os pares de criptomoedas com par em USDT
            exchange_info = requests.get('https://api.binance.com/api/v3/exchangeInfo').json()
            symbols = [symbol_info['symbol'] for symbol_info in exchange_info['symbols'] if symbol_info['quoteAsset'] == 'USDT']

            if usdt_balance > 0 and symbols:
                min_timeframe = 1  # Tempo mínimo para verificar variações (1 minuto)
                best_trade = None

                for symbol in symbols:
                    endpoint = 'klines'
                    params = {
                        'symbol': symbol,
                        'interval': f'{min_timeframe}m',
                        'limit': 10,  # Obtém os últimos 10 minutos de dados
                    }
                    klines = requests.get(base_url + endpoint, params=params).json()

                    # Verifica se a variação nos últimos minutos é maior que 0.05 (5%)
                    close_prices = [float(kline[4]) for kline in klines]
                    variation = (max(close_prices) / min(close_prices)) - 1

                    if variation >= 0.05:
                        best_trade = symbol
                        break

                if best_trade:
                    print(f"Compra de {best_trade} por {get_crypto_price(best_trade)}")
                    buy(best_trade, usdt_balance)  # Utilize todo o saldo em USDT para a compra
                    time.sleep(60)  # Aguarde 1 minuto antes de verificar novamente
                else:
                    time.sleep(60)  # Aguarde 1 minuto antes de verificar novamente

            else:
                print("Saldo insuficiente em USDT para operação.")
                time.sleep(60)  # Aguarde 1 minuto antes de verificar novamente

        except Exception as e:
            print(f"Erro: {e}")

'''