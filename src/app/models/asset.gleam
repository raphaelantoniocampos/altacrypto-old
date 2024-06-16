import birl.{type Time}
import bison/bson
import bison/decoders
import bison/object_id.{type ObjectId}
import gleam/dynamic

pub type Asset {
  Asset(
    id: ObjectId,
    user_id: ObjectId,
    symbol: String,
    quantity: Float,
    purchase_price: Float,
    operation_datetime: Time,
    highest_price: Float,
    current_price: Float,
    should_be_sold: Bool,
  )
}

pub fn bson_decoder() {
  dynamic.decode9(
    Asset,
    dynamic.field("id", decoders.object_id),
    dynamic.field("user_id", decoders.object_id),
    dynamic.field("symbol", decoders.string),
    dynamic.field("quantity", decoders.float),
    dynamic.field("purchase_price", decoders.float),
    dynamic.field("operation_datetime", decoders.time),
    dynamic.field("highest_price", decoders.float),
    dynamic.field("current_price", decoders.float),
    dynamic.field("should_be_sold", decoders.bool),
  )
}
