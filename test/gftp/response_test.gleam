import gftp/response.{Response}
import gftp/status
import gleeunit/should

pub fn describe_with_valid_utf8_message_test() {
  Response(code: status.Ready, message: <<"Welcome to FTP server":utf8>>)
  |> response.describe
  |> should.equal("[service ready for new user] Welcome to FTP server")
}

pub fn describe_with_empty_message_test() {
  Response(code: status.CommandOk, message: <<>>)
  |> response.describe
  |> should.equal("[command okay] ")
}

pub fn describe_with_invalid_utf8_message_test() {
  Response(code: status.NotLoggedIn, message: <<0xFF, 0xFE>>)
  |> response.describe
  |> should.equal("[user not logged in] Invalid UTF-8 in response message")
}

pub fn describe_with_error_status_test() {
  Response(code: status.FileUnavailable, message: <<
    "No such file or directory":utf8,
  >>)
  |> response.describe
  |> should.equal(
    "[requested action not taken; file unavailable] No such file or directory",
  )
}

pub fn describe_with_multiline_message_test() {
  Response(code: status.Help, message: <<"LINE1\r\nLINE2":utf8>>)
  |> response.describe
  |> should.equal("[help message] LINE1\r\nLINE2")
}
