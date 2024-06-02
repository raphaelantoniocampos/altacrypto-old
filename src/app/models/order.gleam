import app/models/asset
import birl/interval
import bison/object_id

pub type Order {
  BuyOrder(
    user_id: object_id.ObjectId,
    side: String,
    interval: interval.Interval,
    symbol: String,
    variation: Float,
    current_price: Float,
  )
  SellOrder(
    user_id: object_id.ObjectId,
    side: String,
    interval: interval.Interval,
    asset: asset.Asset,
  )
}
