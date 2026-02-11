//// This module exposes the FEAT command types

import gftp/result.{type FtpResult} as ftp_result
import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// The features supported by the server, as returned by the FEAT command.
/// 
/// A feat has a key representing the name of the feature, and an optional value representing any parameters for that feature.
pub type Features =
  dict.Dict(String, Option(String))

/// Returns whether the given line is the last line of a FEAT response, which starts with "211 ".
pub fn is_last_line(line: String) -> Bool {
  string.starts_with(line, "211 ")
}

/// Parses the lines of a FEAT response into a `Features` dictionary.
pub fn parse_features(lines: List(String)) -> FtpResult(Features) {
  parse_feature_loop(dict.new(), lines)
}

/// Helper function to recursively parse the lines of a FEAT response into a `Features` dictionary.
fn parse_feature_loop(acc: Features, lines: List(String)) -> FtpResult(Features) {
  case lines {
    [] -> Ok(acc)
    [line, ..rest] -> {
      use #(key, value) <- result.try(parse_feature(line))
      parse_feature_loop(dict.insert(acc, key, value), rest)
    }
  }
}

/// Parses a single line of a FEAT response into a key-value pair.
fn parse_feature(line: String) -> FtpResult(#(String, Option(String))) {
  use trimmed_line <- result.try(trim_line(line))
  let tokens = string.split(trimmed_line, " ")

  case tokens {
    [] -> Error(ftp_result.BadResponse)
    [key] -> Ok(#(key, None))
    [key, ..rest] -> Ok(#(key, Some(string.join(rest, " "))))
  }
}

/// Checks if the given line starts with a space, and if so, trims it. Otherwise, returns an error.
fn trim_line(line: String) -> FtpResult(String) {
  case string.starts_with(line, " ") {
    True -> Ok(string.trim(line))
    False -> Error(ftp_result.BadResponse)
  }
}
