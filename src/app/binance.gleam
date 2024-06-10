import gleam/http/request
import gleam/http/response.{type Response}
import gleam/io
import gleam/uri

const base_url = "https://api.binance.com"

pub fn query_binance_status() {
  let endpoint = "/sapi/v1/system/status"
  let response = make_request(endpoint)
}

fn fetch_usdt_pairs() {
  todo
}

fn make_request(endpoint: String) {
  case uri.parse(base_url <> endpoint) {
    Ok(uri) -> {
      case request.from_uri(uri) {
        Ok(req) -> io.debug(req.body)
        Error(_) -> make_request(endpoint)
      }
    }
    Error(_) -> make_request(endpoint)
  }
}
