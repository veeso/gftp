//// File type options for FTP

import gleam/int

/// Text Format Control used in `TYPE` command
pub type FormatControl {
  /// Default text format control (is NonPrint)
  Default
  /// Non-print (not destined for printing)
  NonPrint
  /// Telnet format control (\<CR\>, \<FF\>, etc.)
  Telnet
  /// ASA (Fortran) Carriage Control
  Asa
}

/// File Type used in `TYPE` command
pub type FileType {
  /// ASCII text (the argument is the text format control)
  Ascii(FormatControl)
  /// EBCDIC text (the argument is the text format control)
  Ebcdic(FormatControl)
  /// Image (binary data)
  Image
  /// Binary (the synonym to Image)
  Binary
  /// Local format (the argument is the number of bits in one byte on local machine)
  Local(Int)
}

fn format_control_to_string(format_control: FormatControl) -> String {
  case format_control {
    Default | NonPrint -> "N"
    Telnet -> "T"
    Asa -> "C"
  }
}

pub fn to_string(file_type: FileType) -> String {
  case file_type {
    Ascii(format_control) -> "A " <> format_control_to_string(format_control)
    Ebcdic(format_control) -> "E " <> format_control_to_string(format_control)
    Image | Binary -> "I"
    Local(bits) -> "L " <> int.to_string(bits)
  }
}
