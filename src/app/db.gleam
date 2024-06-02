import dot_env
import dot_env/env

import mungo

pub fn get_database() {
  let assert Ok(client) =
    mungo.start(
      "mongodb://app-dev:passwd@localhost/app-db?authSource=admin",
      512,
    )

  let users =
    client
    |> mungo.collection("users")

  users
}
