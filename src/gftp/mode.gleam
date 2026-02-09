//// FTP Mode

/// Connection mode for data channel
pub type Mode {
  Active
  Passive
  /// Required by some servers (ipv6); defined in rfc 2428 <https://www.rfc-editor.org/rfc/rfc2428#section-3>
  ExtendedPassive
}
