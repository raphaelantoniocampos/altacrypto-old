import birl
import bison/object_id

pub opaque type Id {
  ObjectId(object_id.ObjectId)
  Null
}

pub opaque type Sold {
  Sold(birl.Time)
  NotSold
}

pub type Asset {
  Asset(
    user_id: Id,
    symbol: String,
    quantity: Float,
    purchase_price: Float,
    purchase_datetime: birl.Time,
    highest_price: Float,
    current_price: Float,
    id: Id,
    should_be_sold: Bool,
    sold: Sold,
  )
}
