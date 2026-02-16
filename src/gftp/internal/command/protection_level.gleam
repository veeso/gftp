//// Protection level types for the FTP PROT command (RFC 2228).

/// Protection level; argument for `Prot` command.
pub type ProtectionLevel {
  Clear
  Private
}

/// Convert a `ProtectionLevel` to its string representation for the `PROT` command.
pub fn to_string(prot: ProtectionLevel) -> String {
  case prot {
    Clear -> "C"
    Private -> "P"
  }
}
