import gftp/command/feat
import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should

// --- is_last_line ---

pub fn is_last_line_with_closing_line_test() {
  feat.is_last_line("211 End") |> should.be_true
}

pub fn is_last_line_with_header_line_test() {
  feat.is_last_line("211-Extensions supported:") |> should.be_false
}

pub fn is_last_line_with_feature_line_test() {
  feat.is_last_line(" AUTH TLS") |> should.be_false
}

// --- parse_features ---

pub fn parse_features_empty_list_test() {
  feat.parse_features([])
  |> should.equal(Ok(dict.new()))
}

pub fn parse_features_single_feature_without_value_test() {
  feat.parse_features([" UTF8"])
  |> should.equal(Ok(dict.from_list([#("UTF8", None)])))
}

pub fn parse_features_single_feature_with_value_test() {
  feat.parse_features([" REST STREAM"])
  |> should.equal(Ok(dict.from_list([#("REST", Some("STREAM"))])))
}

pub fn parse_features_multiple_features_test() {
  feat.parse_features([" AUTH TLS", " PBSZ", " SIZE", " REST STREAM", " UTF8"])
  |> should.equal(
    Ok(
      dict.from_list([
        #("AUTH", Some("TLS")),
        #("PBSZ", None),
        #("SIZE", None),
        #("REST", Some("STREAM")),
        #("UTF8", None),
      ]),
    ),
  )
}

pub fn parse_features_line_without_leading_space_test() {
  feat.parse_features(["AUTH TLS"])
  |> should.be_error
}
