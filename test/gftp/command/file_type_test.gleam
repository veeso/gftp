import gftp/command/file_type.{
  Ascii, Asa, Binary, Default, Ebcdic, Image, Local, NonPrint, Telnet,
}
import gleeunit/should

pub fn ascii_default_test() {
  file_type.to_string(Ascii(Default))
  |> should.equal("A N")
}

pub fn ascii_non_print_test() {
  file_type.to_string(Ascii(NonPrint))
  |> should.equal("A N")
}

pub fn ascii_telnet_test() {
  file_type.to_string(Ascii(Telnet))
  |> should.equal("A T")
}

pub fn ascii_asa_test() {
  file_type.to_string(Ascii(Asa))
  |> should.equal("A C")
}

pub fn ebcdic_default_test() {
  file_type.to_string(Ebcdic(Default))
  |> should.equal("E N")
}

pub fn ebcdic_non_print_test() {
  file_type.to_string(Ebcdic(NonPrint))
  |> should.equal("E N")
}

pub fn ebcdic_telnet_test() {
  file_type.to_string(Ebcdic(Telnet))
  |> should.equal("E T")
}

pub fn ebcdic_asa_test() {
  file_type.to_string(Ebcdic(Asa))
  |> should.equal("E C")
}

pub fn image_test() {
  file_type.to_string(Image)
  |> should.equal("I")
}

pub fn binary_test() {
  file_type.to_string(Binary)
  |> should.equal("I")
}

pub fn local_test() {
  file_type.to_string(Local(8))
  |> should.equal("L 8")
}
