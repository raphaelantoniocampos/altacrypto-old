import dot_env
import dot_env/env

import mungo
import mungo/client.{type Collection}

import gleam/io

fn get_connection_string() -> String {
  dot_env.load()
  let assert Ok(user) = env.get("MONGO_USER")
  let assert Ok(pass) = env.get("MONGO_PASSWORD")
  let assert Ok(db) = env.get("MONGO_DB")

  let string =
    "mongodb://"
    <> user
    <> ":"
    <> pass
    <> "@cluster0.wovexfa.mongodb.net:27017/"
    <> db
    <> "?authSource=admin"
  // io.println(string)
  string
  let new_string =
    "mongodb://admin:dinheiromtechobatmannmuie@cluster0.wovexfa.mongodb.net/altadata?authSource=admin"
  //io.println(new_string)
  //new_string

  "mongodb://127.0.0.1:27017/altadata?authSource=admin&directConnection=true&serverSelectionTimeoutMS=2000&appName=mongosh+2.2.6"
}

pub fn get_collection(name: String) {
  let connection_string = get_connection_string()

  case mungo.start(connection_string, 512) {
    Ok(client) -> {
      client |> mungo.collection(name)
    }
    Error(_) -> get_collection(name)
  }
}
