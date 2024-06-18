import bison/bson.{type Value}
import bison/object_id.{type ObjectId}
import dot_env
import dot_env/env
import gleam/list
import gleam/result
import mungo/crud.{type UpdateResult}

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

pub fn insert_data(
  this list: List(value),
  at collection_name: String,
  with encoder: fn(List(value)) -> List(List(#(String, bson.Value))),
) -> Result(Nil, String) {
  use collection <- result.try(get_collection(collection_name))
  encoder(list)
  |> list.sized_chunk(10)
  |> list.each(fn(a) { mungo.insert_many(collection, a, 1024) })
  |> Ok
}

pub fn get_data(
  from collection_name: String,
  using filter: List(#(String, Value)),
  with decoder: fn(List(Value)) -> Result(List(value), String),
) -> Result(List(value), String) {
  use collection <- result.try(get_collection(collection_name))
  case mungo.find_many(collection, filter, [], 128) {
    Ok(cursor) -> {
      mungo.to_list(cursor, 128)
      |> decoder
    }
    Error(_) -> Error("Error getting documents from: " <> collection_name)
  }
}

pub fn update_one(
  this value: value,
  with id: ObjectId,
  at collection_name: String,
  using encoder: fn(value) -> List(#(String, bson.Value)),
) -> Result(UpdateResult, String) {
  use collection <- result.try(get_collection(collection_name))
  let doc = encoder(value)
  mungo.update_one(collection, [#("_id", bson.ObjectId(id))], doc, [], 512)
  |> result.replace_error("Error updating data" <> object_id.to_string(id))
}
