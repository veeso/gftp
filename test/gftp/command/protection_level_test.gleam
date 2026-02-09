import gftp/command/protection_level.{Clear, Private}
import gleeunit/should

pub fn clear_test() {
  protection_level.to_string(Clear)
  |> should.equal("C")
}

pub fn private_test() {
  protection_level.to_string(Private)
  |> should.equal("P")
}
