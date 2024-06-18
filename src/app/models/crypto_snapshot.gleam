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

pub fn bson_decoder() {
  dynamic.decode3(
    CryptoSnapshot,
    dynamic.field("symbol", decoders.string),
    dynamic.field("datetime", decoders.time),
    dynamic.field("price", decoders.float),
  )
}

pub fn document_decoder(
  values: List(bson.Value),
) -> Result(List(CryptoSnapshot), String) {
  list.try_map(values, fn(value) -> Result(CryptoSnapshot, String) {
    case value {
      bson.Document(doc) -> {
        bison.to_custom_type(doc, bson_decoder())
        |> result.replace_error("Error decoding crypto snapshot")
      }
      _ -> Error("Error decoding crypto snapshot")
    }
  })
}

pub fn bson_encoder(
  list: List(CryptoSnapshot),
) -> List(List(#(String, bson.Value))) {
  list.map(list, fn(crypto_snapshot: CryptoSnapshot) {
    [
      #("symbol", bson.String(crypto_snapshot.symbol)),
      #("datetime", bson.DateTime(crypto_snapshot.datetime)),
      #("price", bson.Double(crypto_snapshot.price)),
    ]
  })
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

pub fn from_tickers(tickers: List(#(String, Float))) -> List(CryptoSnapshot) {
  let now = birl.now()
  list.map(tickers, ticker_to_snapshot(_, now))
  |> list.filter(fn(snapshot: CryptoSnapshot) {
    string.ends_with(snapshot.symbol, "USDT")
  })
}
