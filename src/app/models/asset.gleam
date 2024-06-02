import birl
import bison/object_id

pub type Asset {
  Asset(
    user_id: object_id.ObjectId,
    symbol: String,
    quantity: Float,
    purchase_price: Float,
    purchase_datetime: birl.Time,
    highest_price: Float,
    current_price: Float,
    id: object_id.ObjectId,
    should_be_sold: Bool,
    sold: birl.Time,
  )
}
