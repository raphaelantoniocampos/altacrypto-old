import dot_env
import dot_env/env
import gleam/dynamic
import gleam/list

import mungo
import mungo/client.{type Collection}

import app/models/crypto_snapshot.{type CryptoSnapshot}
import bison/bson
import gleam/io

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

pub fn get_collection(name: String) -> Result(Collection, String) {
  let connection_string = get_connection_string()

  case mungo.start(connection_string, 512) {
    Ok(client) -> {
      client
      |> mungo.collection(name)
      |> Ok
    }
    Error(_) -> {
      Error("Error getting collection: " <> name)
    }
  }
}

fn insert_list(collection, list) {
  case list {
    [] -> Nil
    [head, ..tail] -> {
      let _ = mungo.insert_one(collection, head, 128)
      insert_list(collection, tail)
    }
  }
}

pub fn insert_crypto_snapshots(crypto_snapshots: List(CryptoSnapshot)) {
  let assert Ok(collection) = get_collection("crypto_snapshots")
  let lista = get_document_list("CryptoSnapshot", crypto_snapshots)

  // insert one by one
  insert_list(collection, lista)
  io.debug("Done")
  // insert_many
  // let _ =
  //   collection
  //   |> mungo.insert_many(lista, { 1024 * 1024 })
}

fn convert_to_document(crypto_snapshot: CryptoSnapshot) {
  [
    #("symbol", bson.String(crypto_snapshot.symbol)),
    #("datetime", bson.DateTime(crypto_snapshot.datetime)),
    #("price", bson.Double(crypto_snapshot.price)),
  ]
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
      let doc = convert_to_document(head)
      let list = [doc, ..list]
      document_loop(tail, list)
    }
  }
}
