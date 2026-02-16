//// This module exposes the FEAT command types

import gftp/result.{type FtpError} as ftp_result
import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Returns whether the given line is the last line of a FEAT response, which starts with "211 ".
pub fn is_last_line(line: String) -> Bool {
  string.starts_with(line, "211 ")
}

/// Parses the lines of a FEAT response into a features dictionary.
pub fn parse_features(
  lines: List(String),
) -> Result(dict.Dict(String, Option(String)), FtpError) {
  parse_feature_loop(dict.new(), lines)
}

/// Helper function to recursively parse the lines of a FEAT response into a features dictionary.
fn parse_feature_loop(
  acc: dict.Dict(String, Option(String)),
  lines: List(String),
) -> Result(dict.Dict(String, Option(String)), FtpError) {
  case lines {
    [] -> Ok(acc)
    [line, ..rest] -> {
      use #(key, value) <- result.try(parse_feature(line))
      parse_feature_loop(dict.insert(acc, key, value), rest)
    }
  }
}

/// Parses a single line of a FEAT response into a key-value pair.
fn parse_feature(line: String) -> Result(#(String, Option(String)), FtpError) {
  use trimmed_line <- result.try(trim_line(line))
  let tokens = string.split(trimmed_line, " ")

  case tokens {
    [] -> Error(ftp_result.BadResponse)
    [key] -> Ok(#(key, None))
    [key, ..rest] -> Ok(#(key, Some(string.join(rest, " "))))
  }
}

/// Checks if the given line starts with a space, and if so, trims it. Otherwise, returns an error.
fn trim_line(line: String) -> Result(String, FtpError) {
  case string.starts_with(line, " ") {
    True -> Ok(string.trim(line))
    False -> Error(ftp_result.BadResponse)
  }
}
