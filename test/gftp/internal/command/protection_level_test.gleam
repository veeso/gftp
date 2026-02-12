import gftp/internal/command/protection_level.{Clear, Private}

pub fn clear_test() {
  let assert "C" = protection_level.to_string(Clear)
}

pub fn private_test() {
  let assert "P" = protection_level.to_string(Private)
}
