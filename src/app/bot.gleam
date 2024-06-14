import app/binance
import app/db
import birl
import birl/duration
import gleam/int
import gleam/io
import gleam/result

pub fn start() {
  io.println("Bot started")
  case feed_database() {
    Ok(a) -> io.debug(a)
    Error(err) -> io.debug(err)
  }
}

pub fn feed_database() -> Result(String, String) {
  let name = "feed_database"
  let start = birl.now()

  use pares <- result.try(binance.get_usdt_pairs())

  use _ <- result.try(db.insert_crypto_snapshots(pares))
  let end = birl.now()
  let difference =
    birl.difference(end, start)
    |> duration.blur_to(duration.MilliSecond)

  Ok(name <> " took: " <> int.to_string(difference) <> "ms")
}
