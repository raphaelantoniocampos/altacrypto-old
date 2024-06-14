//// A module for interacting with the Binance API.
////
//// base_url: String - The base URL for the Binance API.

import app/models/crypto_snapshot
import gleam/dynamic
import gleam/http/request
import gleam/http/response.{type Response}
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/result

const base_url = "https://api.binance.com"

fn status_decoder() {
  dynamic.decode2(
    fn(a, b) { #(a, b) },
    dynamic.field("status", dynamic.int),
    dynamic.field("msg", dynamic.string),
  )
}

/// Queries the status of the Binance system.
///
pub fn query_binance_status() -> Result(#(Int, String), String) {
  let endpoint = "/sapi/v1/system/status"
  use resp <- result.try(make_request(endpoint))

  case json.decode(from: resp.body, using: status_decoder()) {
    Ok(status) -> Ok(status)
    Error(_) -> Error("Failed to querry BinanceAPI status")
  }
}

/// Gets the USDT pairs from Binance.
///
pub fn get_usdt_pairs() -> Result(List(crypto_snapshot.CryptoSnapshot), String) {
  case query_binance_status() {
    Ok(status) if status.0 == 0 -> {
      let endpoint = "/api/v3/ticker/price"
      use resp <- result.try(make_request(endpoint))

      case
        json.decode(
          from: resp.body,
          using: dynamic.list(of: crypto_snapshot.ticker_decoder()),
        )
      {
        Ok(tickers) -> {
          tickers
          |> crypto_snapshot.get_snapshots_list
          |> Ok
        }
        Error(_) -> {
          Error("Failed to get USDT pairs from BinanceAPI")
        }
      }
    }
    Ok(status) -> Error("Binance API Error: " <> status.1)
    Error(err) -> Error(err)
  }
}

/// Makes a request to the Binance API.
///
/// Args:
///     endpoint: String -> The API endpoint to query.
fn make_request(endpoint: String) -> Result(Response(String), String) {
  let assert Ok(req) = request.to(base_url <> endpoint)
  let resp_result =
    httpc.send(req)
    |> result.replace_error(
      "Failed to make request do BinanceAPI: " <> endpoint,
    )

  use resp <- result.try(resp_result)

  case resp.status {
    200 -> Ok(resp)
    _ -> {
      io.debug(resp.body)
      Error(
        "Got status "
        <> int.to_string(resp.status)
        <> " from BinanceAPI: "
        <> endpoint,
      )
    }
  }
}
