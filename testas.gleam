fn get_interval_data(
  crypto_snapshots: List(crypto_snapshot.CryptoSnapshot),
  now: birl.Time,
) {
  global_settings.intervals_in_minutes
  |> list.map(fn(interval) { process_interval(interval, crypto_snapshots, now) })
  |> Ok
}

fn process_interval(
  interval: Int,
  crypto_snapshots: List(crypto_snapshot.CryptoSnapshot),
  now: birl.Time,
) {
  list.group(crypto_snapshots, fn(snapshot) { snapshot.symbol })
  |> dict.map_values(fn(_, snapshots) {
    calculate_interval_data(interval, snapshots, now)
  })
  |> filter_valid_entries()
  |> dict.map_values(fn(_, dic) { dict.values(dic) })
}

fn calculate_interval_data(
  interval: Int,
  snapshots: List(crypto_snapshot.CryptoSnapshot),
  now: birl.Time,
) {
  use recent <- result.try(list.first(snapshots))
  use past <- result.try(find_past_snapshot(interval, recent, snapshots))

  let interval_time =
    birl.difference(past.datetime, recent.datetime)
    |> duration.blur_to(duration.Minute)

  dict.new()
  |> dict.insert(
    recent.symbol,
    IntervalData(
      interval_time,
      now,
      recent.price,
      past.price,
      calculate_percentage_change(recent.price, past.price),
    ),
  )
  |> Ok
}

fn find_past_snapshot(
  interval: Int,
  recent: crypto_snapshot.CryptoSnapshot,
  snapshots: List(crypto_snapshot.CryptoSnapshot),
) {
  list.find(snapshots, fn(snap) {
    [interval - 1, interval, interval + 1]
    |> list.contains(
      birl.difference(recent.datetime, snap.datetime)
      |> duration.blur_to(duration.Minute),
    )
  })
}

fn calculate_percentage_change(recent_price: Float, past_price: Float) {
  { { recent_price -. past_price } /. recent_price } *. 100.0
}

fn filter_valid_entries(d) {
  dict.map_values(d, fn(key, result) {
    case result {
      Ok(value) -> {
        dict.new() |> dict.insert(key, value)
      }
      _ -> dict.new()
    }
  })
  |> dict.filter(fn(_, entry) { dict.size(entry) != 0 })
}
