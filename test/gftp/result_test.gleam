import gftp/response.{Response}
import gftp/result
import gftp/status
import gleam/dynamic
import gleeunit/should
import kafein
import mug

pub fn describe_error_connection_error_ipv4_test() {
  result.ConnectionError(mug.ConnectFailedIpv4(mug.Econnrefused))
  |> result.describe_error
  |> should.equal("Connection error: Connection refused")
}

pub fn describe_error_connection_error_ipv6_test() {
  result.ConnectionError(mug.ConnectFailedIpv6(mug.Timeout))
  |> result.describe_error
  |> should.equal("Connection error: Operation timed out")
}

pub fn describe_error_connection_error_both_test() {
  result.ConnectionError(mug.ConnectFailedBoth(
    ipv4: mug.Econnrefused,
    ipv6: mug.Ehostunreach,
  ))
  |> result.describe_error
  |> should.equal("Connection error: Connection refused")
}

pub fn describe_error_secure_error_test() {
  result.SecureError("TLS handshake failed")
  |> result.describe_error
  |> should.equal("Secure error: TLS handshake failed")
}

pub fn describe_error_unexpected_response_test() {
  result.UnexpectedResponse(
    Response(code: status.NotLoggedIn, message: <<"Login required":utf8>>),
  )
  |> result.describe_error
  |> should.equal("Unexpected response: [user not logged in] Login required")
}

pub fn describe_error_bad_response_test() {
  result.BadResponse
  |> result.describe_error
  |> should.equal("Bad response syntax")
}

pub fn describe_error_socket_error_test() {
  result.Socket(mug.Econnreset)
  |> result.describe_error
  |> should.equal("Socket error: Connection reset by peer")
}

pub fn describe_error_data_connection_already_open_test() {
  result.DataConnectionAlreadyOpen
  |> result.describe_error
  |> should.equal("Data connection is already open")
}

pub fn describe_error_tls_closed_test() {
  result.Tls(kafein.Closed)
  |> result.describe_error
  |> should.equal("TLS error: connection closed")
}

pub fn describe_error_tls_timeout_test() {
  result.Tls(kafein.Timeout)
  |> result.describe_error
  |> should.equal("TLS error: timeout reached")
}

pub fn describe_error_tls_other_test() {
  result.Tls(kafein.Other(dynamic.string("some error")))
  |> result.describe_error
  |> should.equal("TLS error: an error occurred during TLS operation")
}

pub fn describe_error_tls_posix_error_test() {
  result.Tls(kafein.PosixError(mug.Econnreset))
  |> result.describe_error
  |> should.equal("TLS error: Connection reset by peer")
}

pub fn describe_error_tls_cipher_suite_not_recognized_test() {
  result.Tls(kafein.CipherSuiteNotRecognized("TLS_RSA_WITH_NULL_MD5"))
  |> result.describe_error
  |> should.equal(
    "TLS error: cipher suite not recognized: TLS_RSA_WITH_NULL_MD5",
  )
}

pub fn describe_error_tls_alert_test() {
  result.Tls(kafein.TlsAlert(
    kind: kafein.HandshakeFailure,
    description: "handshake failed",
  ))
  |> result.describe_error
  |> should.equal("TLS error: TLS alert: handshake failed")
}
