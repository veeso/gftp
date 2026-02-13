//// FTP Mode

/// Connection mode for data channel
pub type Mode {
  /// Active mode with a timeout for the data connection.
  /// The timeout is used to prevent hanging connections in active mode, where the server connects back to the client for data transfer.
  /// If the server doesn't connect within the specified timeout, the client can close the connection and report an error.
  Active(timeout: Int)
  Passive
  /// Required by some servers (ipv6); defined in rfc 2428 <https://www.rfc-editor.org/rfc/rfc2428#section-3>
  ExtendedPassive
}

/// IP version for use with the EPRT command (RFC 2428).
pub type IpVersion {
  V4
  V6
}
