import app/global_settings
import birl.{type Time}
import bison
import bison/bson.{type Value, Document}
import bison/decoders
import bison/object_id.{type ObjectId}
import gleam/dynamic
import gleam/float
import gleam/int
import gleam/io
import gleam/result

pub type Asset {
  Asset(
    id: ObjectId,
    user_id: ObjectId,
    symbol: String,
    quantity: Float,
    purchase_price: Float,
    datetime: Time,
    highest_price: Float,
    current_price: Float,
    should_be_sold: Bool,
  )
}

pub fn bson_decoder() {
  dynamic.decode9(
    Asset,
    dynamic.field("_id", decoders.object_id),
    dynamic.field("user_id", decoders.object_id),
    dynamic.field("symbol", decoders.string),
    dynamic.field("quantity", decoders.float),
    dynamic.field("purchase_price", decoders.float),
    dynamic.field("datetime", decoders.time),
    dynamic.field("highest_price", decoders.float),
    dynamic.field("current_price", decoders.float),
    dynamic.field("should_be_sold", decoders.bool),
  )
}

pub fn values_to_asset(values: List(Value)) -> List(Asset) {
  values_loop(values, [])
  |> result.values
}

fn values_loop(values: List(Value), list) {
  case values {
    [] -> list
    [head, ..tail] -> {
      case to_asset(head) {
        asset -> values_loop(tail, [asset, ..list])
      }
    }
  }
}

fn to_asset(value) -> Result(Asset, String) {
  case value {
    Document(doc) -> {
      bison.to_custom_type(doc, bson_decoder())
      |> result.replace_error("Error decoding asset")
    }
    _ -> Error("Error decoding asset")
  }
}

pub fn update_asset(asset: Asset, price: Float) -> Asset {
  let highest_price = case price {
    n if n >. asset.highest_price -> price
    _ -> asset.highest_price
  }
  let variation = {
    { { price -. asset.purchase_price } /. price } *. 100.0
  }
  let variation_under_highest = {
    highest_price *. { 1.0 -. { global_settings.sell_under_purchase /. 100.0 } }
  }
  let should_be_sold = case price, variation {
    _, var if var <=. -3.0 -> True
    _, var if var >=. 200.0 -> True
    price, _ if price <=. variation_under_highest -> True
    _, _ -> False
  }

  Asset(
    id: asset.id,
    user_id: asset.user_id,
    symbol: asset.symbol,
    quantity: asset.quantity,
    purchase_price: asset.purchase_price,
    datetime: asset.datetime,
    highest_price: highest_price,
    current_price: price,
    should_be_sold: should_be_sold,
  )
}
