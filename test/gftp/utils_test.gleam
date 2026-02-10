import gftp/result as ftp_result
import gftp/utils.{extract_str, parse_int, parse_month}
import gleam/time/calendar
import gleeunit/should

pub fn extract_quoted_str() {
  extract_str("The file is \"file.txt\" and it's ready for download", "\"")
  |> should.equal(Ok("file.txt"))
}

pub fn extract_quoted_str_within_multiple_tokens() {
  extract_str(
    "The file is \"file.txt\" and it's ready for download. Please check the \"file.txt\" file.",
    "\"",
  )
  |> should.equal(Ok(
    "file.txt\" and it's ready for download. Please check the \"file.txt",
  ))
}

pub fn extract_str_with_different_token() {
  extract_str("The file is <file.txt> and it's ready for download", "<")
  |> should.equal(Ok("file.txt> and it's ready for download"))
}

pub fn extract_str_with_token_not_present() {
  extract_str("The file is file.txt and it's ready for download", "\"")
  |> should.equal(Error(Nil))
}

pub fn parse_int_valid() {
  parse_int("123")
  |> should.equal(Ok(123))
}

pub fn parse_int_invalid() {
  parse_int("abc")
  |> should.equal(Error(ftp_result.BadResponse))
}

pub fn parse_month_valid() {
  parse_month("1")
  |> should.equal(Ok(calendar.January))
}

pub fn parse_month_invalid() {
  parse_month("13")
  |> should.equal(Error(ftp_result.BadResponse))
}
