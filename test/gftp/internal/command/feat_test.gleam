import gftp/internal/command/feat
import gleam/dict
import gleam/option.{None, Some}

// --- is_last_line ---

pub fn is_last_line_with_closing_line_test() {
  let assert True = feat.is_last_line("211 End")
}

pub fn is_last_line_with_header_line_test() {
  let assert False = feat.is_last_line("211-Extensions supported:")
}

pub fn is_last_line_with_feature_line_test() {
  let assert False = feat.is_last_line(" AUTH TLS")
}

// --- parse_features ---

pub fn parse_features_empty_list_test() {
  let assert Ok(result) = feat.parse_features([])
  let assert True = dict.to_list(result) == dict.to_list(dict.new())
}

pub fn parse_features_single_feature_without_value_test() {
  let assert Ok(result) = feat.parse_features([" UTF8"])
  let assert True =
    dict.to_list(result) == dict.to_list(dict.from_list([#("UTF8", None)]))
}

pub fn parse_features_single_feature_with_value_test() {
  let assert Ok(result) = feat.parse_features([" REST STREAM"])
  let assert True =
    dict.to_list(result)
    == dict.to_list(dict.from_list([#("REST", Some("STREAM"))]))
}

pub fn parse_features_multiple_features_test() {
  let assert Ok(result) =
    feat.parse_features([" AUTH TLS", " PBSZ", " SIZE", " REST STREAM", " UTF8"])
  let assert True =
    dict.to_list(result)
    == dict.to_list(
      dict.from_list([
        #("AUTH", Some("TLS")),
        #("PBSZ", None),
        #("SIZE", None),
        #("REST", Some("STREAM")),
        #("UTF8", None),
      ]),
    )
}

pub fn parse_features_line_without_leading_space_test() {
  let assert Error(_) = feat.parse_features(["AUTH TLS"])
}
