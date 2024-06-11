import birl
import gleam/dynamic
import gleam/float
import gleam/io
import gleam/list
import gleam/string

pub type CryptoSnapshot {
  CryptoSnapshot(symbol: String, datetime: birl.Time, price: Float)
}

pub type Ticker {
  Ticker(symbol: String, price: String)
}

pub fn ticker_decoder() {
  dynamic.decode2(
    Ticker,
    dynamic.field("symbol", dynamic.string),
    dynamic.field("price", dynamic.string),
  )
}

fn ticker_to_snapshot(
  ticker: Ticker,
  current_datetime: birl.Time,
) -> CryptoSnapshot {
  case float.parse(ticker.price) {
    Ok(price) -> {
      CryptoSnapshot(
        symbol: ticker.symbol,
        datetime: current_datetime,
        price: price,
      )
    }
    Error(_) -> CryptoSnapshot("", current_datetime, 0.0)
  }
}

pub fn filter_usdt(snapshots: List(CryptoSnapshot)) -> List(CryptoSnapshot) {
  let usdt = fn(snapshot: CryptoSnapshot) {
    string.ends_with(snapshot.symbol, "USDT")
  }
  list.filter(snapshots, usdt)
}

pub fn get_snapshots_list(tickers: List(Ticker)) -> List(CryptoSnapshot) {
  let now = birl.now()
  tickers_loop(tickers, [], now)
  |> filter_usdt
}

fn tickers_loop(
  tickers: List(Ticker),
  crypto_snapshots: List(CryptoSnapshot),
  now: birl.Time,
) {
  case tickers {
    [] -> crypto_snapshots
    [head, ..tail] -> {
      let crypto = ticker_to_snapshot(head, now)
      let crypto_snapshots = [crypto, ..crypto_snapshots]
      tickers_loop(tail, crypto_snapshots, now)
    }
  }
}
