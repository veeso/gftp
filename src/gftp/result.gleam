//// The result module exposes the Result type and errors related to gftp.

import gftp/response.{type Response}
import kafein
import mug

/// The FtpResult type is a Result that can contain either a successful value of type a or an FtpError.
pub type FtpResult(a) =
  Result(a, FtpError)

pub type FtpError {
  /// Connection error
  ConnectionError(mug.ConnectError)
  /// Unexpected response from remote. The command expected a certain response, but got another one.
  /// This means the ftp server refused to perform your request or there was an error while processing it.
  /// Contains the response data.
  UnexpectedResponse(Response)
  /// The response syntax is invalid
  BadResponse
  /// An error occurred during a TLS handshake or while sending/receiving data over a TLS stream.
  Tls(kafein.Error)
  /// The address provided was invalid
  Socket(mug.Error)
}

fn connection_error_to_string(e: mug.ConnectError) -> String {
  case e {
    mug.ConnectFailedIpv4(e) -> mug.describe_error(e)
    mug.ConnectFailedIpv6(e) -> mug.describe_error(e)
    mug.ConnectFailedBoth(e, _) -> mug.describe_error(e)
  }
}

/// The description function takes an FtpError and returns a human-readable string describing the error.
pub fn describe_error(err: FtpError) -> String {
  case err {
    ConnectionError(e) -> "Connection error: " <> connection_error_to_string(e)
    UnexpectedResponse(r) -> "Unexpected response: " <> response.describe(r)
    BadResponse -> "Bad response syntax"
    Socket(e) -> "Socket error: " <> mug.describe_error(e)
    Tls(e) ->
      "TLS error"
      <> case e {
        kafein.Closed -> ": connection closed"
        kafein.Timeout -> ": timeout reached"
        kafein.Other(_) -> ": an error occurred during TLS operation"
        kafein.PosixError(e) -> ": " <> mug.describe_error(e)
        kafein.CipherSuiteNotRecognized(e) ->
          ": cipher suite not recognized: " <> e
        kafein.TlsAlert(_, description) -> ": TLS alert: " <> description
      }
  }
}
