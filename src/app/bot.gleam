import app/binance
import app/db
import app/global_settings
import app/models/asset.{type Asset}
import app/models/crypto_snapshot.{type CryptoSnapshot}
import birl
import birl/duration
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import pprint

type IntervalData {
  IntervalData(
    interval_time: Int,
    datetime: birl.Time,
    recent_price: Float,
    past_price: Float,
    percentage_change: Float,
  )
}

pub fn start() {
  io.println("Bot started")
  use tickers <- result.try(binance.get_usdt_tickers())

  feed_database(tickers)
  |> io.debug

  update_assets(tickers)
  |> result.unwrap([])
  |> list.length
  |> io.debug

  use crypto_snapshots <- result.try(db.get_data(
    "crypto_snapshots",
    [],
    crypto_snapshot.document_decoder,
  ))

  let now = birl.now()
  get_interval_data(crypto_snapshots, now)
  // |> pprint.debug

  Ok("end")
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

fn get_interval_data(
  crypto_snapshots: List(crypto_snapshot.CryptoSnapshot),
  now: birl.Time,
) {
  global_settings.intervals_in_minutes
  |> list.map(fn(interval) { process_interval(interval, crypto_snapshots, now) })
  |> Ok
}

fn process_interval(
  interval: Int,
  crypto_snapshots: List(crypto_snapshot.CryptoSnapshot),
  now: birl.Time,
) {
  list.group(crypto_snapshots, fn(snapshot) { snapshot.symbol })
  |> dict.map_values(fn(_, snapshots) {
    calculate_interval_data(interval, snapshots, now)
  })
  |> filter_valid_entries()
  |> dict.map_values(fn(_, dic) { dict.values(dic) })
}

fn calculate_interval_data(
  interval: Int,
  snapshots: List(crypto_snapshot.CryptoSnapshot),
  now: birl.Time,
) {
  use recent <- result.try(list.first(snapshots))
  use past <- result.try(find_past_snapshot(interval, recent, snapshots))

  let interval_time =
    birl.difference(past.datetime, recent.datetime)
    |> duration.blur_to(duration.Minute)

  dict.new()
  |> dict.insert(
    recent.symbol,
    IntervalData(
      interval_time,
      now,
      recent.price,
      past.price,
      calculate_percentage_change(recent.price, past.price),
    ),
  )
  |> Ok
}

fn find_past_snapshot(
  interval: Int,
  recent: crypto_snapshot.CryptoSnapshot,
  snapshots: List(crypto_snapshot.CryptoSnapshot),
) {
  list.find(snapshots, fn(snap) {
    [interval - 1, interval, interval + 1]
    |> list.contains(
      birl.difference(recent.datetime, snap.datetime)
      |> duration.blur_to(duration.Minute),
    )
  })
}

fn calculate_percentage_change(recent_price: Float, past_price: Float) {
  { { recent_price -. past_price } /. recent_price } *. 100.0
}

fn filter_valid_entries(d) {
  dict.map_values(d, fn(key, result) {
    case result {
      Ok(value) -> {
        dict.new() |> dict.insert(key, value)
      }
      _ -> dict.new()
    }
  })
  |> dict.filter(fn(_, entry) { dict.size(entry) != 0 })
}
