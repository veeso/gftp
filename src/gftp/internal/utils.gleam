import gftp/response.{type Response}
import gftp/result.{type FtpResult} as ftp_result
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/regexp
import gleam/result
import gleam/string
import gleam/time/calendar

/// Get the substring enclosed in the provided token.
pub fn extract_str(body: String, token: String) -> Result(String, Nil) {
  case string.split(body, token) {
    [_, ..rest] -> {
      rest
      |> list.take(list.length(rest) - 1)
      |> string.join(token)
      |> Ok
    }
    _ -> Error(Nil)
  }
}

/// A helper function to match a regular expression against a string and extract the submatches as a list of strings.
pub fn re_matches(
  re: regexp.Regexp,
  line: String,
) -> Result(List(Option(String)), Nil) {
  case regexp.scan(re, line) {
    [] -> Error(Nil)
    matches ->
      matches
      |> list.map(fn(match) { match.submatches })
      |> list.flatten()
      |> Ok
  }
}

/// A helper function to parse a string into an integer, returning an ftp error if the parsing fails.
pub fn parse_int(s: String) -> FtpResult(Int) {
  case int.parse(s) {
    Ok(i) -> Ok(i)
    Error(_) -> Error(ftp_result.BadResponse)
  }
}

/// A helper function to parse a string into a Month
pub fn parse_month(s: String) -> FtpResult(calendar.Month) {
  use num <- result.try(parse_int(s))

  case calendar.month_from_int(num) {
    Ok(month) -> Ok(month)
    Error(_) -> Error(ftp_result.BadResponse)
  }
}

/// A helper function to convert a Response to a String, returning an ftp error if the conversion fails.
pub fn response_to_string(response: Response) -> FtpResult(String) {
  response
  |> response.to_string
  |> result.map_error(fn(_) { ftp_result.BadResponse })
}

/// A helper function to check if a string is an IPv6 address by checking for the presence of a colon.
pub fn is_ipv6_address(s: String) -> Bool {
  string.contains(s, ":")
}
