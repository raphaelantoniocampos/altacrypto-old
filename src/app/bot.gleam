import app/binance
import app/db
import app/global_settings
import app/models/asset.{type Asset}
import app/models/crypto_snapshot.{type CryptoSnapshot}
import birl
import birl/duration
import birl/interval
import bison/object_id.{type ObjectId}
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import pprint

type VariationData {
  VariationData(
    symbol: String,
    interval_time: Int,
    datetime: birl.Time,
    recent_price: Float,
    past_price: Float,
    variation: Float,
  )
}

type Order {
  BuyOrder(
    user_id: ObjectId,
    side: String,
    interval: interval.Interval,
    symbol: String,
    variation: Float,
    current_price: Float,
  )
  SellOrder(user_id: ObjectId, side: String, asset: Asset)
}

pub fn start() {
  io.println("Bot started")
  use tickers <- result.try(binance.get_usdt_tickers())

  let _feed_result = feed_database(tickers)

  use assets <- result.try(update_assets(tickers))
  let sell_assets = get_sell_orders(assets)
  pprint.debug(sell_assets)

  use crypto_snapshots <- result.try(db.get_data(
    "crypto_snapshots",
    [],
    crypto_snapshot.document_decoder,
  ))

  let now = birl.now()
  let interval_data = get_interval_data(crypto_snapshots, now)

  Ok("end")
}

fn feed_database(tickers: List(#(String, Float))) -> Result(String, String) {
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

  use updated_assets <- result.try(asset.update_assets_with_tickers(
    assets,
    tickers,
  ))
  let _update_result =
    list.try_map(updated_assets, fn(asset) {
      db.update_one(asset, asset.id, "assets", asset.bson_encoder)
    })
  Ok(updated_assets)
}

fn get_interval_data(
  crypto_snapshots: List(CryptoSnapshot),
  now: birl.Time,
) -> List(Result(#(Int, Float, List(VariationData)), Nil)) {
  global_settings.intervals_in_minutes
  |> list.map(fn(interval) {
    list.group(crypto_snapshots, fn(snapshot) { snapshot.symbol })
    |> dict.map_values(fn(symbol, snapshots) {
      use recent <- result.try(list.first(snapshots))
      use past <- result.try(find_past_snapshot(interval, recent, snapshots))

      let interval_time =
        birl.difference(past.datetime, recent.datetime)
        |> duration.blur_to(duration.Minute)

      VariationData(symbol, interval_time, now, recent.price, past.price, {
        { recent.price -. past.price } /. recent.price
      })
      |> Ok
    })
    |> dict.map_values(fn(_, result) {
      result.lazy_unwrap(result, fn() {
        VariationData("", 0, now, 0.0, 0.0, 0.0)
      })
    })
    |> dict.filter(fn(_, data) { data.symbol != "" })
    |> dict.values
  })
  |> list.map(fn(interval_list) {
    let mean_variation = calculate_mean_variation(interval_list)
    use first_of_list <- result.try(list.first(interval_list))
    let interval_time = first_of_list.interval_time
    #(interval_time, mean_variation, interval_list)
    |> Ok
  })
}

fn find_past_snapshot(
  interval: Int,
  recent: CryptoSnapshot,
  snapshots: List(CryptoSnapshot),
) {
  list.find(snapshots, fn(snap) {
    [interval - 1, interval, interval + 1]
    |> list.contains(
      birl.difference(recent.datetime, snap.datetime)
      |> duration.blur_to(duration.Minute),
    )
  })
}

fn calculate_mean_variation(interval_list: List(VariationData)) {
  let sum =
    list.fold(interval_list, 0.0, fn(acc, data) { acc +. data.variation })
  sum /. int.to_float(list.length(interval_list))
}

fn get_sell_orders(assets: List(Asset)) {
  list.filter(assets, fn(asset) { asset.should_be_sold })
  |> list.map(fn(asset) {
    SellOrder(user_id: asset.user_id, side: "Sell", asset: asset)
  })
}

fn get_buy_orders(interval_data) {
  todo
}
