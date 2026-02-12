import gftp/file_type.{
  Asa, Ascii, Binary, Default, Ebcdic, Image, Local, NonPrint, Telnet,
}

pub fn ascii_default_test() {
  let assert "A N" = file_type.to_string(Ascii(Default))
}

pub fn ascii_non_print_test() {
  let assert "A N" = file_type.to_string(Ascii(NonPrint))
}

pub fn ascii_telnet_test() {
  let assert "A T" = file_type.to_string(Ascii(Telnet))
}

pub fn ascii_asa_test() {
  let assert "A C" = file_type.to_string(Ascii(Asa))
}

pub fn ebcdic_default_test() {
  let assert "E N" = file_type.to_string(Ebcdic(Default))
}

pub fn ebcdic_non_print_test() {
  let assert "E N" = file_type.to_string(Ebcdic(NonPrint))
}

pub fn ebcdic_telnet_test() {
  let assert "E T" = file_type.to_string(Ebcdic(Telnet))
}

pub fn ebcdic_asa_test() {
  let assert "E C" = file_type.to_string(Ebcdic(Asa))
}

pub fn image_test() {
  let assert "I" = file_type.to_string(Image)
}

pub fn binary_test() {
  let assert "I" = file_type.to_string(Binary)
}

pub fn local_test() {
  let assert "L 8" = file_type.to_string(Local(8))
}
