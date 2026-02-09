//// This module contains the types and functions related to file permissions,
//// such as read, write, and execute permissions for the owner, group, and others.

import gleam/int

/// The `FilePermissions` type represents the permissions for a file or directory in a POSIX file system.
pub type FilePermissions {
  FilePermissions(
    owner: PosixPermission,
    group: PosixPermission,
    others: PosixPermission,
  )
}

/// The `PosixPermission` type represents the permissions for a group of users (owner, group, or others) in a POSIX file system.
/// It contains three boolean fields: `read`, `write`, and `execute`.
pub type PosixPermission {
  PosixPermission(read: Bool, write: Bool, execute: Bool)
}

/// Get the value of a specific bit from an integer, where `position` is the index of the bit (0 for the least significant bit).
fn bit_from_int(value: Int, position: Int) -> Bool {
  { value |> int.bitwise_shift_right(position) |> int.bitwise_and(0x01) } != 0
}

/// Converts an integer value (0-7) to a `PosixPermission` type, where the bits represent the read, write, and execute permissions.
pub fn from_int(value: Int) -> PosixPermission {
  let read = bit_from_int(value, 2)
  let write = bit_from_int(value, 1)
  let execute = bit_from_int(value, 0)

  PosixPermission(read, write, execute)
}
