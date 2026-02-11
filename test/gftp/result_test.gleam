import gftp/response.{Response}
import gftp/result
import gftp/status
import gleam/dynamic
import kafein
import mug

pub fn describe_error_connection_error_ipv4_test() {
  let assert "Connection error: Connection refused" =
    result.ConnectionError(mug.ConnectFailedIpv4(mug.Econnrefused))
    |> result.describe_error
}

pub fn describe_error_connection_error_ipv6_test() {
  let assert "Connection error: Operation timed out" =
    result.ConnectionError(mug.ConnectFailedIpv6(mug.Timeout))
    |> result.describe_error
}

pub fn describe_error_connection_error_both_test() {
  let assert "Connection error: Connection refused" =
    result.ConnectionError(mug.ConnectFailedBoth(
      ipv4: mug.Econnrefused,
      ipv6: mug.Ehostunreach,
    ))
    |> result.describe_error
}

pub fn describe_error_unexpected_response_test() {
  let assert "Unexpected response: [user not logged in] Login required" =
    result.UnexpectedResponse(
      Response(code: status.NotLoggedIn, message: <<"Login required":utf8>>),
    )
    |> result.describe_error
}

pub fn describe_error_bad_response_test() {
  let assert "Bad response syntax" =
    result.BadResponse
    |> result.describe_error
}

pub fn describe_error_socket_error_test() {
  let assert "Socket error: Connection reset by peer" =
    result.Socket(mug.Econnreset)
    |> result.describe_error
}

pub fn describe_error_tls_closed_test() {
  let assert "TLS error: connection closed" =
    result.Tls(kafein.Closed)
    |> result.describe_error
}

pub fn describe_error_tls_timeout_test() {
  let assert "TLS error: timeout reached" =
    result.Tls(kafein.Timeout)
    |> result.describe_error
}

pub fn describe_error_tls_other_test() {
  let assert "TLS error: an error occurred during TLS operation" =
    result.Tls(kafein.Other(dynamic.string("some error")))
    |> result.describe_error
}

pub fn describe_error_tls_posix_error_test() {
  let assert "TLS error: Connection reset by peer" =
    result.Tls(kafein.PosixError(mug.Econnreset))
    |> result.describe_error
}

pub fn describe_error_tls_cipher_suite_not_recognized_test() {
  let assert "TLS error: cipher suite not recognized: TLS_RSA_WITH_NULL_MD5" =
    result.Tls(kafein.CipherSuiteNotRecognized("TLS_RSA_WITH_NULL_MD5"))
    |> result.describe_error
}

pub fn describe_error_tls_alert_test() {
  let assert "TLS error: TLS alert: handshake failed" =
    result.Tls(kafein.TlsAlert(
      kind: kafein.HandshakeFailure,
      description: "handshake failed",
    ))
    |> result.describe_error
}
