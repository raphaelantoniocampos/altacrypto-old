import app/binance
import app/bot
import app/db
import app/models/crypto_snapshot
import app/router
import app/web.{Context}
import birl
import dot_env
import dot_env/env
import gleam/erlang/process
import gleam/io
import gleam/list
import mist
import mungo
import wisp

pub fn main() {
  wisp.configure_logger()

  dot_env.load()
  let assert Ok(secret_key_base) = env.get("SECRET_KEY_BASE")

  let ctx = Context(static_directory: static_directory())

  let handler = router.handle_request(_, ctx)

  let assert Ok(_) =
    wisp.mist_handler(handler, secret_key_base)
    |> mist.new
    |> mist.port(8000)
    |> mist.start_http

  let start_bot = bot.start
  process.start(start_bot, True)

  let assert Ok(pares) = binance.get_usdt_pairs()
  pares
  |> db.insert_crypto_snapshots

  process.sleep_forever()
}

fn static_directory() {
  let assert Ok(priv_directory) = wisp.priv_directory("altacrypto")
  priv_directory <> "/static"
}
