//// Error types for gftp operations.
////
//// All gftp functions return `Result(a, FtpError)`.
////
//// ```gleam
//// import gftp
//// import gftp/result
////
//// case gftp.cwd(client, "/nonexistent") {
////   Ok(_) -> // success
////   Error(err) -> {
////     let msg = result.describe_error(err)
////     // "Unexpected response: [requested action not taken; file unavailable] ..."
////   }
//// }
//// ```

import gftp/response.{type Response}
import kafein
import mug

/// Error type returned by all gftp operations.
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
  /// A data transfer is in progress; close the data channel before issuing control commands
  DataTransferInProgress
}

fn connection_error_to_string(e: mug.ConnectError) -> String {
  case e {
    mug.ConnectFailedIpv4(e) -> mug.describe_error(e)
    mug.ConnectFailedIpv6(e) -> mug.describe_error(e)
    mug.ConnectFailedBoth(e, _) -> mug.describe_error(e)
  }
}

/// Convert an `FtpError` to a human-readable string.
///
/// ```gleam
/// let msg = result.describe_error(err)
/// // e.g. "Connection error: Connection refused"
/// // e.g. "Unexpected response: [user not logged in] Login required"
/// ```
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
    DataTransferInProgress ->
      "Data transfer in progress: close the data channel before issuing control commands"
  }
}
