import app/db
import app/global_settings
import birl.{type Time}
import bison
import bison/bson
import bison/decoders
import bison/object_id.{type ObjectId}
import gleam/dict
import gleam/dynamic
import gleam/list
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

pub fn document_decoder(values: List(bson.Value)) -> Result(List(Asset), String) {
  list.try_map(values, fn(value) -> Result(Asset, String) {
    case value {
      bson.Document(doc) -> {
        bison.to_custom_type(doc, bson_decoder())
        |> result.replace_error("Error decoding asset")
      }
      _ -> Error("Error decoding asset")
    }
  })
}

pub fn bson_encoder(asset: Asset) -> List(#(String, bson.Value)) {
  [
    #("_id", bson.ObjectId(asset.id)),
    #("user_id", bson.ObjectId(asset.user_id)),
    #("symbol", bson.String(asset.symbol)),
    #("quantity", bson.Double(asset.quantity)),
    #("purchase_price", bson.Double(asset.purchase_price)),
    #("highest_price", bson.Double(asset.highest_price)),
    #("current_price", bson.Double(asset.current_price)),
    #("should_be_sold", bson.Boolean(asset.should_be_sold)),
    #("datetime", bson.DateTime(asset.datetime)),
  ]
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

pub fn update_assets_with_tickers(
  assets: List(Asset),
  values: List(#(String, Float)),
) -> Result(List(Asset), String) {
  let tickers = dict.from_list(values)
  list.try_map(assets, fn(asset: Asset) {
    case dict.get(tickers, asset.symbol) {
      Ok(price) -> {
        let updated_asset = update_asset(asset, price)
        let _update_result =
          db.update_one(updated_asset, updated_asset.id, "assets", bson_encoder)
        updated_asset
        |> Ok
      }
      Error(_) -> Error("Error updating assets")
    }
  })
}
