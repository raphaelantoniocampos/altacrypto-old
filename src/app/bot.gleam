import app/binance
import app/db
import app/global_settings
import app/models/asset.{type Asset}
import app/models/crypto_snapshot.{type CryptoSnapshot}
import birl
import birl/duration
import birl/interval
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/order
import gleam/result

pub fn start() {
  io.println("Bot started")
  use tickers <- result.try(binance.get_usdt_tickers())

  feed_database(tickers)
  |> io.debug

  update_assets(tickers)
  // |> io.debug

  get_intervals_data()
  |> io.debug
}

pub fn feed_database(tickers: List(#(String, Float))) -> Result(String, String) {
  let name = "feed_database"
  let start = birl.now()
  use _ <- result.try(
    crypto_snapshot.from_tickers(tickers)
    |> db.insert_data("crypto_snapshots", crypto_snapshot.bson_encoder),
  )
  let end = birl.now()
  let difference =
    birl.difference(end, start)
    |> duration.blur_to(duration.MilliSecond)

  Ok(name <> " took: " <> int.to_string(difference) <> "ms")
}

fn update_assets(tickers: List(#(String, Float))) -> Result(List(Asset), String) {
  use assets <- result.try(db.get_data("assets", [], asset.document_decoder))

  asset.update_assets_with_tickers(assets, tickers)
}

fn get_intervals_data() {
  use crypto_snapshots <- result.try(db.get_data(
    "crypto_snapshots",
    [],
    crypto_snapshot.document_decoder,
  ))

  let now = birl.now()

  list.map(global_settings.intervals_in_minutes, fn(interval: Int) {
    dict.map_values(
      list.group(crypto_snapshots, fn(snapshot: CryptoSnapshot) {
        snapshot.symbol
      }),
      fn(symbol: String, list: List(crypto_snapshot.CryptoSnapshot)) {
        use recent <- result.try(list.first(list))
        use past <- result.try(
          list.find(list, fn(snap: crypto_snapshot.CryptoSnapshot) {
            let difference =
              birl.difference(recent.datetime, snap.datetime)
              |> duration.blur_to(duration.Minute)
            [interval - 1, interval, interval + 1]
            |> list.contains(difference)
          }),
        )
        let interval_time =
          birl.difference(past.datetime, recent.datetime)
          |> duration.blur_to(duration.Minute)

        dict.new()
        |> dict.insert(
          symbol,
          #(interval_time, now, recent.price, past.price, {
            { { recent.price -. past.price } /. recent.price } *. 100.0
          }),
        )
        |> Ok
      },
    )
    |> dict.filter(fn(_, b) {
      case b {
        Error(Nil) -> True
        _ -> False
      }
    })
  })
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
  |> Ok
}
