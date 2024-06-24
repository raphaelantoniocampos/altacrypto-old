import app/models/asset.{Asset}
import app/models/crypto_snapshot
import birl
import bison/object_id
import gleam/io
import gleam/list
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}

pub fn from_tickers_test() {
  let ls =
    crypto_snapshot.from_tickers([#("SYMBOLUSDT", 1.0), #("SYMBOL", 0.5)])
  let assert Ok(frst) = list.first(ls)
  should.equal(list.length(ls), 1)
  should.equal(frst.symbol, "SYMBOLUSDT")
  should.equal(frst.price, 1.0)
}

pub fn update_asset_with_tickers_test() {
  let assert Ok(updated_assets) =
    asset.update_assets_with_tickers(
      [
        Asset(
          id: object_id.new(),
          user_id: object_id.new(),
          symbol: "SYMBOLUSDT",
          quantity: 1.0,
          purchase_price: 100.0,
          datetime: birl.now(),
          highest_price: 120.0,
          current_price: 120.0,
          should_be_sold: False,
        ),
        Asset(
          id: object_id.new(),
          user_id: object_id.new(),
          symbol: "TESTUSDT",
          quantity: 2.0,
          purchase_price: 200.0,
          datetime: birl.now(),
          highest_price: 200.0,
          current_price: 200.0,
          should_be_sold: True,
        ),
      ],
      [#("SYMBOLUSDT", 200.0), #("TESTUSDT", 100.0)],
    )

  let assert Ok(asset_one) = list.first(updated_assets)
  let assert Ok(asset_two) = list.last(updated_assets)

  should.equal(asset_one.symbol, "SYMBOLUSDT")
  should.equal(asset_one.quantity, 1.0)
  should.equal(asset_one.purchase_price, 100.0)
  should.equal(asset_one.highest_price, 200.0)
  should.equal(asset_one.current_price, 200.0)
  should.equal(asset_one.should_be_sold, False)

  should.equal(asset_two.symbol, "TESTUSDT")
  should.equal(asset_two.quantity, 2.0)
  should.equal(asset_two.purchase_price, 200.0)
  should.equal(asset_two.highest_price, 200.0)
  should.equal(asset_two.current_price, 100.0)
  should.equal(asset_two.should_be_sold, True)
}
