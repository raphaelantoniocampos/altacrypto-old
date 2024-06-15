import bison/bson
import dot_env
import dot_env/env
import gleam/io
import gleam/list
import gleam/result

import mungo
import mungo/client.{type Collection}
import mungo/crud

import app/models/crypto_snapshot

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

  "mongodb://127.0.0.1:27017/altadata?authSource=admin&directConnection=true&serverSelectionTimeoutMS=2000&appName=mongosh+2.2.6"
}

pub fn get_collection(name: String) -> Result(Collection, String) {
  let connection_string = get_connection_string()

  use client <- result.try(
    mungo.start(connection_string, 1024)
    |> result.replace_error("Error getting collection: " <> name),
  )
  mungo.collection(client, name)
  |> Ok
}

// CryptoSnapshots

pub fn insert_crypto_snapshots(
  crypto_snapshots: List(crypto_snapshot.CryptoSnapshot),
) -> Result(Nil, String) {
  use collection <- result.try(get_collection("crypto_snapshots"))
  crypto_snapshot.to_documents(crypto_snapshots)
  |> list.sized_chunk(5)
  |> list.each(fn(a) { mungo.insert_many(collection, a, 128) })
  |> Ok
}

// Assets

pub fn get_assets() {
  use collection <- result.try(get_collection("assets"))
  case mungo.find_all(collection, [], 128) {
    Ok(cursor) -> {
      mungo.to_list(cursor, 128)
      |> Ok
    }
    Error(_) -> Error("Error getting assets")
  }
}
