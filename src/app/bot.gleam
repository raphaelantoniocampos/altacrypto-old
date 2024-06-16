import app/binance
import app/db
import app/models/asset.{type Asset}
import app/models/crypto_snapshot.{type CryptoSnapshot}
import birl
import birl/duration
import bison
import gleam/dict
import gleam/dynamic
import gleam/int
import gleam/io
import gleam/list
import gleam/result

pub fn start() {
  io.println("Bot started")
  use crypto_snapshots <- result.try(binance.get_usdt_pairs())

  let _ = feed_database(crypto_snapshots)

  use assets <- result.try(db.get_assets([]))
  use asset <- result.try(
    list.pop(assets, fn(_) { True })
    |> result.replace_error("Error getting assets"),
  )
  let doc = asset.0
  io.debug(doc)

  bison.to_custom_type(doc, asset.bson_decoder)

  //io.debug(asset.1)
  Ok(asset)
}

/// Updates the database with crypto snapshots.
///
/// Args:
/// crypto_snapshots (List(CryptoSnapshot)): containing asset pairs and their prices.
pub fn feed_database(
  crypto_snapshots: List(CryptoSnapshot),
) -> Result(String, String) {
  let name = "feed_database"
  let start = birl.now()

  use _ <- result.try(db.insert_crypto_snapshots(crypto_snapshots))
  let end = birl.now()
  let difference =
    birl.difference(end, start)
    |> duration.blur_to(duration.MilliSecond)

  Ok(name <> " took: " <> int.to_string(difference) <> "ms")
}

/// Updates asset information in the database based on crypto snapshots.
///
/// Args:
/// crypto_snapshots (List(CryptoSnapshot)): A list of CryptoSnapshot objects.
fn update_assets(crypto_snapshots: List(CryptoSnapshot)) -> List(Asset) {
  todo
  // try:
  //     assets = self.get_assets({"sold": None})
  //     crypto_snapshots_dict = {
  //         crypto_snapshot.symbol: crypto_snapshot
  //         for crypto_snapshot in crypto_snapshots
  //     }
  //     updated_assets = []
  //     for asset in assets:
  //         if asset.symbol in crypto_snapshots_dict:
  //             updated_asset = asset.update_asset(
  //                 crypto_snapshots_dict[asset.symbol].price)
  //             self._update_asset(updated_asset)
  //             updated_assets.append(updated_asset)
  //     return updated_assets
  // except pymongo.errors.PyMongoError as e:
  //     self.logger.info(
  //         f"Error updating assets: {e}"
  //     )
  //     return []
}
