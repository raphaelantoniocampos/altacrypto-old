import app/bot
import app/router
import app/web.{Context}
import dot_env
import dot_env/env
import gleam/erlang/process
import mist
import wisp

pub const sell_under_purchase = 3.0

pub const buy_percentage_threshold = 10.0

pub const sell_under_highest = 3.0

pub const sell_above_purchase = 200.0

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

  process.start(bot.start, False)

  process.sleep_forever()
}

fn static_directory() {
  let assert Ok(priv_directory) = wisp.priv_directory("altacrypto")
  priv_directory <> "/static"
}
