import app/binance
import app/db
import app/models/asset.{type Asset}
import app/models/crypto_snapshot.{type CryptoSnapshot}
import birl
import birl/duration
import bison
import bison/bson
import gleam/dict
import gleam/dynamic
import gleam/int
import gleam/io
import gleam/list
import gleam/result

pub fn start() {
  io.println("Bot started")
  use tickers <- result.try(binance.get_usdt_tickers())

  feed_database(tickers)
  |> io.debug

  let assets = update_assets(tickers)
  io.debug(assets)
}

pub fn feed_database(tickers: List(#(String, Float))) -> Result(String, String) {
  let name = "feed_database"
  let start = birl.now()
  crypto_snapshot.get_snapshots_list(tickers)
  use _ <- result.try(
    crypto_snapshot.get_snapshots_list(tickers)
    |> db.insert_crypto_snapshots,
  )
  let end = birl.now()
  let difference =
    birl.difference(end, start)
    |> duration.blur_to(duration.MilliSecond)

  Ok(name <> " took: " <> int.to_string(difference) <> "ms")
}

fn update_assets(tickers: List(#(String, Float))) -> Result(List(Asset), String) {
  use assets <- result.try(db.get_assets([]))

  dict.from_list(tickers)
  |> update_assets_loop(assets, _, [])
  |> Ok
}

fn update_assets_loop(
  assets: List(Asset),
  tickers: dict.Dict(String, Float),
  updated_assets: List(Asset),
) -> List(Asset) {
  case assets {
    [] -> updated_assets
    [head, ..tail] -> {
      case dict.get(tickers, head.symbol) {
        Ok(price) -> {
          let updated_asset = asset.update_asset(head, price)
          db.update_asset(updated_asset)
          update_assets_loop(tail, tickers, [updated_asset, ..updated_assets])
        }
        Error(_) -> update_assets_loop(tail, tickers, updated_assets)
      }
    }
  }
}
