import birl.{type Time}

pub type CryptoSnapshot {
  CryptoSnapshot(symbol: String, datetime: Time, price: Float)
}
