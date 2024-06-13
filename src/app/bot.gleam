import app/binance
import app/db
import birl
import birl/duration
import gleam/erlang/process
import gleam/int
import gleam/io

pub fn start() {
  io.println("Bot started")
  feed_database()
  // let start_time = time.monotonic()
  // Lógica do bot, por exemplo, iniciar a negociação de criptomoedas
  // io.debug(time)
  // Simulação de um trabalho do bot
  // process.sleep(time.seconds(5))

  // let end_time = time.monotonic()
  // let elapsed_time = end_time - start_time
  // let remaining_time = execution_frequency() - elapsed_time

  // case remaining_time {
  // time if time > 0 -> process.sleep(remaining_time)
  // _ -> io.debug()
  // }

  // io.println("Bot running")
}

pub fn feed_database() -> Result(String, String) {
  let name = "database_feeder"
  let start = birl.now()
  case binance.get_usdt_pairs() {
    Ok(pares) -> {
      db.insert_crypto_snapshots(pares)
      let end = birl.now()
      let difference =
        birl.difference(end, start) |> duration.blur_to(duration.MilliSecond)
      io.println(name <> " took: " <> int.to_string(difference) <> "ms")
      Ok("Ok")
    }
    Error(err) -> Error(err)
  }
}

fn loop(interval seconds: Int) {
  let _ = feed_database()

  process.sleep(seconds * 1000)
  loop(seconds)
}
