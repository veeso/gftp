import gftp/list/permission.{PosixPermission}

pub fn from_int_no_permissions_test() {
  let assert PosixPermission(read: False, write: False, execute: False) =
    permission.from_int(0)
}

pub fn from_int_execute_only_test() {
  let assert PosixPermission(read: False, write: False, execute: True) =
    permission.from_int(1)
}

pub fn from_int_write_only_test() {
  let assert PosixPermission(read: False, write: True, execute: False) =
    permission.from_int(2)
}

pub fn from_int_write_execute_test() {
  let assert PosixPermission(read: False, write: True, execute: True) =
    permission.from_int(3)
}

pub fn from_int_read_only_test() {
  let assert PosixPermission(read: True, write: False, execute: False) =
    permission.from_int(4)
}

pub fn from_int_read_execute_test() {
  let assert PosixPermission(read: True, write: False, execute: True) =
    permission.from_int(5)
}

pub fn from_int_read_write_test() {
  let assert PosixPermission(read: True, write: True, execute: False) =
    permission.from_int(6)
}

pub fn from_int_all_permissions_test() {
  let assert PosixPermission(read: True, write: True, execute: True) =
    permission.from_int(7)
}
