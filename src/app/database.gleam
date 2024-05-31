import mungo

pub fn main() {
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
