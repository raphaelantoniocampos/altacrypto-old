import birl
import bison
import bison/bson
import bison/decoders
import gleam/dynamic
import gleam/float
import gleam/list
import gleam/result
import gleam/string

pub type CryptoSnapshot {
  CryptoSnapshot(symbol: String, datetime: birl.Time, price: Float)
}

pub fn ticker_decoder() {
  dynamic.decode2(
    fn(a, b) {
      case float.parse(b) {
        Ok(b) -> #(a, b)
        Error(_) -> #("", 0.0)
      }
    },
    dynamic.field("symbol", dynamic.string),
    dynamic.field("price", dynamic.string),
  )
}

fn ticker_to_snapshot(
  ticker: #(String, Float),
  current_datetime: birl.Time,
) -> CryptoSnapshot {
  CryptoSnapshot(symbol: ticker.0, datetime: current_datetime, price: ticker.1)
}

pub fn filter_usdt(snapshots: List(CryptoSnapshot)) -> List(CryptoSnapshot) {
  let usdt = fn(snapshot: CryptoSnapshot) {
    string.ends_with(snapshot.symbol, "USDT")
  }
  list.filter(snapshots, usdt)
}

pub fn get_snapshots_list(
  tickers: List(#(String, Float)),
) -> List(CryptoSnapshot) {
  let now = birl.now()
  tickers_loop(tickers, [], now)
  |> filter_usdt
}

fn tickers_loop(
  tickers: List(#(String, Float)),
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

pub fn to_documents(list: List(CryptoSnapshot)) {
  document_loop(list, [])
}

fn document_loop(documents, list) {
  case documents {
    [] -> list
    [head, ..tail] -> {
      let doc = to_document(head)
      let list = [doc, ..list]
      document_loop(tail, list)
    }
  }
}

fn to_document(crypto_snapshot: CryptoSnapshot) {
  [
    #("symbol", bson.String(crypto_snapshot.symbol)),
    #("datetime", bson.DateTime(crypto_snapshot.datetime)),
    #("price", bson.Double(crypto_snapshot.price)),
  ]
}

pub fn values_to_crypto_snapshot(
  values: List(bson.Value),
) -> List(CryptoSnapshot) {
  values_loop(values, [])
  |> result.values
}

fn values_loop(values: List(bson.Value), list) {
  case values {
    [] -> list
    [head, ..tail] -> {
      case to_crypto_snapshot(head) {
        asset -> values_loop(tail, [asset, ..list])
      }
    }
  }
}

fn to_crypto_snapshot(value) -> Result(CryptoSnapshot, String) {
  case value {
    bson.Document(doc) -> {
      bison.to_custom_type(doc, bson_decoder())
      |> result.replace_error("Error decoding crypto snapshot")
    }
    _ -> Error("Error decoding crypto snapshot")
  }
}

pub fn bson_decoder() {
  dynamic.decode3(
    CryptoSnapshot,
    dynamic.field("symbol", decoders.string),
    dynamic.field("datetime", decoders.time),
    dynamic.field("price", decoders.float),
  )
}
