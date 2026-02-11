import gftp/response.{Response}
import gftp/status

pub fn describe_with_valid_utf8_message_test() {
  let assert "[service ready for new user] Welcome to FTP server" =
    Response(code: status.Ready, message: <<"Welcome to FTP server":utf8>>)
    |> response.describe
}

pub fn describe_with_empty_message_test() {
  let assert "[command okay] " =
    Response(code: status.CommandOk, message: <<>>)
    |> response.describe
}

pub fn describe_with_invalid_utf8_message_test() {
  let assert "[user not logged in] Invalid UTF-8 in response message" =
    Response(code: status.NotLoggedIn, message: <<0xFF, 0xFE>>)
    |> response.describe
}

pub fn describe_with_error_status_test() {
  let assert "[requested action not taken; file unavailable] No such file or directory" =
    Response(code: status.FileUnavailable, message: <<
      "No such file or directory":utf8,
    >>)
    |> response.describe
}

pub fn describe_with_multiline_message_test() {
  let assert "[help message] LINE1\r\nLINE2" =
    Response(code: status.Help, message: <<"LINE1\r\nLINE2":utf8>>)
    |> response.describe
}
