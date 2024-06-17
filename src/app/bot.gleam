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

  update_assets(tickers)
  |> io.debug

  get_intervals_data()
  |> io.debug
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
  |> db.update_assets_loop(assets, _, [])
  |> Ok
}

fn get_intervals_data() {
  db.get_crypto_snapshots([])
  // intervals_dataframe = []
  // all_crypto_snapshots = self.database_manager.get_all_crypto_snapshots()
  // crypto_snapshots_by_symbol = self._separate_crypto_snapshots_by_symbol(
  //     all_crypto_snapshots
  // )
  // current_datetime = datetime.now()
  // for interval_in_minutes in GlobalSettings.INTERVALS_IN_MINUTES:
  //     interval_dataframe = self._get_interval_dataframe(
  //         interval_in_minutes, crypto_snapshots_by_symbol, current_datetime
  //     )
  //     if not interval_dataframe.empty:
  //         intervals_dataframe.append(interval_dataframe)
  // return intervals_dataframe
}
