import app/binance
import app/db
import birl
import birl/duration
import gleam/int
import gleam/io

pub fn start() {
  // let start_time = time.monotonic()
  // Lógica do bot, por exemplo, iniciar a negociação de criptomoedas
  io.println("Bot started")

  let name = "start bot"
  let start = birl.now()
  let assert Ok(pares) = binance.get_usdt_pairs()
  pares
  |> db.insert_crypto_snapshots
  let end = birl.now()
  let difference =
    birl.difference(end, start) |> duration.blur_to(duration.MilliSecond)
  io.println(name <> " took: " <> int.to_string(difference) <> "ms")
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
