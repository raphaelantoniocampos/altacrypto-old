import birl
import bison/object_id

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
