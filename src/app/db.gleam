import dot_env
import dot_env/env
import gleam/io
import gleam/list
import gleam/queue
import gleam/result

import mungo
import mungo/client.{type Collection}

import app/models/crypto_snapshot.{type CryptoSnapshot}
import bison/bson

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

fn get_document_list(format: String, list: List(CryptoSnapshot)) {
  case format {
    "CryptoSnapshot" -> {
      document_loop(list, [])
    }
    _ -> []
  }
}

fn document_loop(documents, list) {
  case documents {
    [] -> list
    [head, ..tail] -> {
      let doc = snapshot_to_document(head)
      let list = [doc, ..list]
      document_loop(tail, list)
    }
  }
}

// CryptoSnapshots

pub fn insert_crypto_snapshots(
  crypto_snapshots: List(CryptoSnapshot),
) -> Result(Nil, String) {
  use collection <- result.try(get_collection("crypto_snapshots"))
  // get_document_list("CryptoSnapshot", crypto_snapshots)
  // |> list.sized_chunk(5)
  // |> list.each(fn(a) { mungo.insert_many(collection, a, 128) })
  // |> Ok

  get_document_list("CryptoSnapshot", crypto_snapshots)
  |> list.each(fn(a) { mungo.insert_one(collection, a, 128) })
  |> Ok
}

fn snapshot_to_document(crypto_snapshot: CryptoSnapshot) {
  [
    #("symbol", bson.String(crypto_snapshot.symbol)),
    #("datetime", bson.DateTime(crypto_snapshot.datetime)),
    #("price", bson.Double(crypto_snapshot.price)),
  ]
}
/// Assets
