import gftp/internal/utils.{extract_str, is_ipv6_address, parse_int, parse_month}
import gftp/result as ftp_result
import gleam/time/calendar

pub fn extract_quoted_str_test() {
  let assert Ok("file.txt") =
    extract_str("The file is \"file.txt\" and it's ready for download", "\"")
}

pub fn extract_quoted_str_within_multiple_tokens_test() {
  let assert Ok(
    "file.txt\" and it's ready for download. Please check the \"file.txt",
  ) =
    extract_str(
      "The file is \"file.txt\" and it's ready for download. Please check the \"file.txt\" file.",
      "\"",
    )
}

pub fn extract_str_with_different_token_test() {
  let assert Ok("file.txt> and it's ready for download") =
    extract_str("The file is <file.txt> and it's ready for download", "<")
}

pub fn extract_str_with_token_not_present_test() {
  let assert Error(Nil) =
    extract_str("The file is file.txt and it's ready for download", "\"")
}

pub fn parse_int_valid_test() {
  let assert Ok(123) = parse_int("123")
}

pub fn parse_int_invalid_test() {
  let assert Error(ftp_result.BadResponse) = parse_int("abc")
}

pub fn parse_month_valid_test() {
  let assert Ok(calendar.January) = parse_month("1")
}

pub fn parse_month_invalid_test() {
  let assert Error(ftp_result.BadResponse) = parse_month("13")
}

pub fn is_ipv6_address_with_ipv4_test() {
  let assert False = is_ipv6_address("192.168.1.1")
}

pub fn is_ipv6_address_with_loopback_v6_test() {
  let assert True = is_ipv6_address("::1")
}

pub fn is_ipv6_address_with_full_v6_test() {
  let assert True = is_ipv6_address("2001:0db8:85a3:0000:0000:8a2e:0370:7334")
}

pub fn is_ipv6_address_with_empty_string_test() {
  let assert False = is_ipv6_address("")
}
