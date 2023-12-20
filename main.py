import os
import time
from datetime import datetime, timedelta
import pandas as pd
from dotenv import load_dotenv
from binance.client import Client
from sqlalchemy import create_engine, Column, String, Float, DateTime
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker

load_dotenv()

api_key = os.environ.get('BINANCE_API_KEY_TEST')
api_secret = os.environ.get('BINANCE_API_SECRET_TEST')

client = Client(api_key, api_secret, testnet=False)

Base = declarative_base()

class CoinPrice(Base):
    __tablename__ = 'coin_prices'

    id = Column(String, primary_key=True)
    symbol = Column(String)
    price = Column(Float)
    timestamp = Column(DateTime)

# Configurar conexão com o banco de dados SQLite
database_url = 'sqlite:///coin_prices.db'
engine = create_engine(database_url)
Base.metadata.create_all(engine)

Session = sessionmaker(bind=engine)

def get_all_prices():
    tickers = client.get_all_tickers()
    df = pd.DataFrame(tickers)
    usdt_pairs = df[df['symbol'].str.endswith("USDT")].reset_index(drop=True)
    return usdt_pairs

def main():
    start_time = datetime.now()
    while True:
        now = datetime.now()
        interval = now - start_time

        if interval >= timedelta(seconds=60):
            prices = get_all_prices()

            with Session() as session:
                for _, row in prices.iterrows():
                    coin_price = CoinPrice(
                        id=row['symbol'],
                        symbol=row['symbol'],
                        price=float(row['price']),
                        timestamp=now
                    )
                    session.add(coin_price)

                session.commit()

            start_time = datetime.now()
            time.sleep(60)  # Aguardar 1 minuto antes de verificar novamente

if __name__ == "__main__":
    main()