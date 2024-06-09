import dot_env
import dot_env/env

import mungo
import mungo/client.{type Collection}
import mungo/error

import gleam/io
import gleam/result

fn get_connection_string() -> String {
  dot_env.load()
  let assert Ok(user) = env.get("MONGO_USER")
  let assert Ok(pass) = env.get("MONGO_PASSWORD")
  let assert Ok(db) = env.get("MONGO_DB")

  let _string =
    "mongodb://"
    <> user
    <> ":"
    <> pass
    <> "@cluster0.wovexfa.mongodb.net:27017/"
    <> db
    <> "?authSource=admin"

  let new_string =
    "mongodb://127.0.0.1:27017/altadata?authSource=admin&directConnection=true&serverSelectionTimeoutMS=2000&appName=mongosh+2.2.6"
  new_string
}

pub fn get_collection(name: String) -> Result(Collection, Nil) {
  let connection_string = get_connection_string()

  let client = mungo.start(connection_string, 512)
  case client {
    Ok(client) -> {
      let col = client |> mungo.collection(name)
      Ok(col)
    }
    Error(_) -> Error(Nil)
  }
}
