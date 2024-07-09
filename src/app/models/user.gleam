import birl
import bison
import bison/bson
import bison/decoders
import bison/object_id
import gleam/dynamic
import gleam/io
import gleam/list
import gleam/result
import glesha

pub type User {
  User(
    id: object_id.ObjectId,
    login: String,
    name: String,
    api_key: String,
    secret_key: String,
    user_settings: UserSettings,
    usd_balance: Float,
    created_at: birl.Time,
    // bytes
    hashed_password: String,
  )
}

pub type UserSettings {
  UserSettings(
    testing: Bool,
    operation_value_percentage: Float,
    maximum_operation_value: Float,
  )
}

pub fn bson_decoder() {
  dynamic.decode9(
    User,
    dynamic.field("_id", decoders.object_id),
    dynamic.field("login", decoders.string),
    dynamic.field("name", decoders.string),
    dynamic.field("api_key", decoders.string),
    dynamic.field("secret_key", decoders.string),
    dynamic.field("user_settings", user_settings_decoder()),
    dynamic.field("usd_balance", decoders.float),
    dynamic.field("created_at", decoders.time),
    dynamic.field("hashed_password", hashed_password_decoder),
  )
}

fn user_settings_decoder() {
  dynamic.decode3(
    UserSettings,
    dynamic.field("testing", decoders.bool),
    dynamic.field("operation_value_percentage", decoders.float),
    dynamic.field("maximum_operation_value", decoders.float),
  )
  // Ok(UserSettings(
  //   testing: True,
  //   operation_value_percentage: 5.0,
  //   maximum_operation_value: 100.0,
  // ))
}

fn hashed_password_decoder(dyn: dynamic.Dynamic) {
  let b = dynamic.bit_array(dyn)
  // Ok(glesha.encode_hex(b))
  Ok("hashed_password")
}
// pub fn test_decoder() {
//   let document =
//     // Document BSON aqui, convertido para Dynamic
//     dynamic.from_bson(
//       Document(
//         dict.from_list([
//           #("maximum_operation_value", Double(100.0)),
//           #("operation_value_percentage", Double(5.0)),
//           #("testing", Boolean(True)),
//         ]),
//       ),
//     )
//
//   case user_settings_decoder()(document) {
//     Ok(user_settings) -> gleam.print(user_settings)
//     Error(error) -> gleam.print(error)
//   }
// }
