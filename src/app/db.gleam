import app/models/asset.{type Asset}
import bison/bson.{type Value}
import bison/object_id
import dot_env
import dot_env/env
import gleam/dict
import gleam/list
import gleam/result
import mungo/crud.{type UpdateResult}

import mungo
import mungo/client.{type Collection}

import app/models/crypto_snapshot.{type CryptoSnapshot}

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
  crypto_snapshots: List(CryptoSnapshot),
) -> Result(Nil, String) {
  use collection <- result.try(get_collection("crypto_snapshots"))
  crypto_snapshot.to_documents(crypto_snapshots)
  |> list.sized_chunk(10)
  |> list.each(fn(a) { mungo.insert_many(collection, a, 1024) })
  |> Ok
}

pub fn get_crypto_snapshots(
  filter: List(#(String, Value)),
) -> Result(List(CryptoSnapshot), String) {
  use collection <- result.try(get_collection("crypto_snapshots"))
  case mungo.find_many(collection, filter, [], 128) {
    Ok(cursor) -> {
      mungo.to_list(cursor, 128)
      |> crypto_snapshot.values_to_crypto_snapshot
      |> Ok
    }
    Error(_) -> Error("Error getting crypto snapshots")
  }
}

// Assets

pub fn get_assets(filter: List(#(String, Value))) -> Result(List(Asset), String) {
  use collection <- result.try(get_collection("assets"))
  case mungo.find_many(collection, filter, [], 128) {
    Ok(cursor) -> {
      mungo.to_list(cursor, 128)
      |> asset.values_to_asset
      |> Ok
    }
    Error(_) -> Error("Error getting assets")
  }
}

pub fn update_asset(asset: Asset) -> Result(UpdateResult, String) {
  use collection <- result.try(get_collection("assets"))
  let doc = asset.to_document(asset)
  mungo.update_one(
    collection,
    [#("_id", bson.ObjectId(asset.id))],
    doc,
    [],
    512,
  )
  |> result.replace_error(
    "Error updating asset" <> object_id.to_string(asset.id),
  )
}

pub fn update_assets_loop(
  assets: List(Asset),
  tickers: dict.Dict(String, Float),
  updated_assets: List(Asset),
) -> List(Asset) {
  case assets {
    [] -> updated_assets
    [head, ..tail] -> {
      case dict.get(tickers, head.symbol) {
        Ok(price) -> {
          let updated_asset = asset.update_asset(head, price)
          update_asset(updated_asset)
          update_assets_loop(tail, tickers, [updated_asset, ..updated_assets])
        }
        Error(_) -> update_assets_loop(tail, tickers, updated_assets)
      }
    }
  }
}
