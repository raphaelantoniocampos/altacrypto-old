import dot_env
import dot_env/env

import gleam/otp/actor.{type StartError}
import mungo
import mungo/client.{type Collection}

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

  let _python_con =
    "mongodb+srv://admin:dinheiromtechobatmannmuie@cluster0.wovexfa.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0"

  let _trying =
    "mongodb://admin:dinheiromtechobatmannmuie@cluster0.wovexfa.mongodb.net:27017/altadata?authSource=admin&retryWrites=true&w=majority&appName=Cluster0"

  let new_string =
    "mongodb://127.0.0.1:27017/altadata?authSource=admin&directConnection=true&serverSelectionTimeoutMS=2000&appName=mongosh+2.2.6"
  new_string
}

pub fn get_collection(name: String) -> Result(Collection, StartError) {
  let connection_string = get_connection_string()

  case mungo.start(connection_string, 512) {
    Ok(client) -> {
      let col = client |> mungo.collection(name)
      Ok(col)
    }
    Error(e) -> {
      Error(e)
    }
  }
}

/// Get collection
pub fn insert_crypto_snapshot() {
  todo
}
