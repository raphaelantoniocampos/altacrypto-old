// The intervals, in minutes, used for data collection and analysis.
pub const intervals_in_minutes: List(Int) = [5, 10, 15, 30, 60]

// The frequency, in minutes, at which the application executes its operations.
pub const execution_frequency_minutes: Float = 1.0

// The threshold percentage used for buying operations.
pub const buy_percentage_threshold: Float = 1.0

// The percentage below which an asset's price is considered for selling if it is below the purchase price.
pub const sell_under_purchase: Float = 3.0

// The percentage below which an asset's price is considered for selling if it is below the highest price reached.
pub const sell_under_highest: Float = 3.0

// The percentage above which an asset's price is considered for selling if it is above the purchase price.
pub const sell_above_purchase: Float = 200.0
