import app/binance
import app/db
import app/global_settings
import app/models/asset.{type Asset}
import app/models/crypto_snapshot.{type CryptoSnapshot}
import birl
import birl/duration
import bison/object_id.{type ObjectId}
import gleam/bool
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import pprint

type PriceVariation {
  PriceVariation(
    symbol: String,
    interval_time: Int,
    datetime: birl.Time,
    recent_price: Float,
    past_price: Float,
    variation: Float,
  )
}

type IntervalData {
  IntervalData(
    interval_time: Int,
    variation_percentage: Float,
    variation_data: List(PriceVariation),
  )
}

type Order {
  BuyOrder(side: String, symbol: String, variation: Float, current_price: Float)
  SellOrder(user_id: ObjectId, side: String, asset: Asset)
}

pub fn start() {
  io.println("Bot started")
  use tickers <- result.try(binance.get_usdt_tickers())

  let _feed_result = feed_database(tickers)

  use assets <- result.try(update_assets(tickers))
  let sell_orders = get_sell_orders(assets)
  // pprint.debug(sell_orders)

  use crypto_snapshots <- result.try(db.get_data(
    "crypto_snapshots",
    [],
    crypto_snapshot.document_decoder,
  ))

  let current_datetime = birl.now()
  let interval_data =
    calculate_interval_data(crypto_snapshots, current_datetime)

  // let buy_orders = get_buy_orders(interval_data)

  pprint.debug(interval_data)
  // pprint.debug(buy_orders)

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

fn calculate_interval_data(
  crypto_snapshots: List(CryptoSnapshot),
  current_datetime: birl.Time,
) -> List(Result(IntervalData, IntervalData)) {
  list.map(global_settings.intervals_in_minutes, fn(interval_minutes) {
    let symbol_snapshots =
      group_snapshots_by_symbol(crypto_snapshots)
      |> dict.map_values(fn(symbol, snapshots) {
        use recent_snapshot <- result.try(list.first(snapshots))
        use past_snapshot <- result.try(find_past_snapshot(
          interval_minutes,
          recent_snapshot,
          snapshots,
        ))

        let interval_duration =
          birl.difference(past_snapshot.datetime, recent_snapshot.datetime)
          |> duration.blur_to(duration.Minute)

        PriceVariation(
          symbol,
          interval_duration,
          current_datetime,
          recent_snapshot.price,
          past_snapshot.price,
          {
            { recent_snapshot.price -. past_snapshot.price }
            /. recent_snapshot.price
          },
        )
        |> Ok
      })
      |> dict.map_values(fn(_, result) {
        result.lazy_unwrap(result, fn() {
          PriceVariation("", 0, current_datetime, 0.0, 0.0, 0.0)
        })
      })
      |> dict.filter(fn(_, data) { data.symbol != "" })
      |> dict.values
    #(interval_minutes, symbol_snapshots)
  })
  |> list.map(fn(interval_data) {
    case interval_data.1 {
      [] -> Error(IntervalData(interval_data.0, 0.0, []))
      _ -> {
        case list.first(interval_data.1) {
          Ok(first_snapshot) -> {
            let average_variation = calculate_average_variation(interval_data.1)
            Ok(IntervalData(
              first_snapshot.interval_time,
              average_variation,
              interval_data.1,
            ))
          }
          _ -> Error(IntervalData(interval_data.0, 0.0, []))
        }
      }
    }
  })
}

fn group_snapshots_by_symbol(
  snapshots: List(CryptoSnapshot),
) -> dict.Dict(String, List(CryptoSnapshot)) {
  list.group(snapshots, fn(snapshot) { snapshot.symbol })
}

fn process_symbol_snapshots(
  interval_minutes: Int,
  current_datetime: birl.Time,
) -> fn(dict.Dict(String, List(CryptoSnapshot))) ->
  dict.Dict(String, Result(PriceVariation, PriceVariation)) {
  dict.map_values(_, fn(symbol, snapshots) {
    use recent_snapshot <- result.try(list.first(snapshots))
    use past_snapshot <- result.try(find_past_snapshot(
      interval_minutes,
      recent_snapshot,
      snapshots,
    ))

    let interval_duration =
      birl.difference(past_snapshot.datetime, recent_snapshot.datetime)
      |> duration.blur_to(duration.Minute)

    PriceVariation(
      symbol,
      interval_duration,
      current_datetime,
      recent_snapshot.price,
      past_snapshot.price,
      calculate_price_change(recent_snapshot.price, past_snapshot.price),
    )
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

fn calculate_price_change(recent_price: Float, past_price: Float) -> Float {
  { recent_price -. past_price } /. recent_price
}

fn filter_valid_snapshots(
  symbol_snapshots: dict.Dict(String, Result(PriceVariation, PriceVariation)),
  current_datetime: birl.Time,
) -> List(PriceVariation) {
  dict.map_values(symbol_snapshots, fn(_, result) {
    result.lazy_unwrap(result, fn() {
      PriceVariation("", 0, current_datetime, 0.0, 0.0, 0.0)
    })
  })
  |> dict.filter(fn(_, data) { data.symbol != "" })
  |> dict.values
}

fn calculate_average_variation(interval_list: List(PriceVariation)) {
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

fn get_buy_orders(
  interval_data: List(Result(IntervalData, Nil)),
) -> List(Result(List(Order), String)) {
  list.map(interval_data, fn(result) {
    case result {
      Ok(interval_data) -> {
        list.filter(interval_data.variation_data, fn(data) {
          data.variation
          >=. {
            interval_data.variation_percentage
            +. global_settings.buy_percentage_threshold
          }
        })
        |> list.map(fn(coin_data) {
          BuyOrder(
            "BUY",
            coin_data.symbol,
            coin_data.variation,
            coin_data.recent_price,
          )
        })
        |> Ok
      }
      Error(_) -> Error("Interval Data not found")
    }
  })
}
