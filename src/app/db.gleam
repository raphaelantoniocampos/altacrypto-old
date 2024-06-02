import dot_env
import dot_env/env

import mungo
import mungo/client.{type Collection}

import gleam/io

fn get_connection_string() -> String {
  dot_env.load()
  let assert Ok(user) = env.get("MONGO_USER")
  let assert Ok(pass) = env.get("MONGO_PASSWORD")

  "mongodb+srv://{mongo_user}:{
  mongo_password}@cluster0.wovexfa.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0"

  "mongodb+srv://"
  <> user
  <> ":"
  <> pass
  <> "@cluster0.wovexfa.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0"
}

pub fn get_collection(name: String) -> Collection {
  let connection_string = get_connection_string()

  let assert Ok(client) = mungo.start(connection_string, 512)
  client |> mungo.collection(name)
}
