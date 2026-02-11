import gftp/result as ftp_result
import gftp/utils.{extract_str, parse_int, parse_month}
import gleam/time/calendar

pub fn extract_quoted_str() {
  let assert Ok("file.txt") =
    extract_str("The file is \"file.txt\" and it's ready for download", "\"")
}

pub fn extract_quoted_str_within_multiple_tokens() {
  let assert Ok(
    "file.txt\" and it's ready for download. Please check the \"file.txt",
  ) =
    extract_str(
      "The file is \"file.txt\" and it's ready for download. Please check the \"file.txt\" file.",
      "\"",
    )
}

pub fn extract_str_with_different_token() {
  let assert Ok("file.txt> and it's ready for download") =
    extract_str("The file is <file.txt> and it's ready for download", "<")
}

pub fn extract_str_with_token_not_present() {
  let assert Error(Nil) =
    extract_str("The file is file.txt and it's ready for download", "\"")
}

pub fn parse_int_valid() {
  let assert Ok(123) = parse_int("123")
}

pub fn parse_int_invalid() {
  let assert Error(ftp_result.BadResponse) = parse_int("abc")
}

pub fn parse_month_valid() {
  let assert Ok(calendar.January) = parse_month("1")
}

pub fn parse_month_invalid() {
  let assert Error(ftp_result.BadResponse) = parse_month("13")
}
