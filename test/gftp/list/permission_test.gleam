import gftp/list/permission.{PosixPermission}
import gleeunit/should

pub fn from_int_no_permissions_test() {
  permission.from_int(0)
  |> should.equal(PosixPermission(read: False, write: False, execute: False))
}

pub fn from_int_execute_only_test() {
  permission.from_int(1)
  |> should.equal(PosixPermission(read: False, write: False, execute: True))
}

pub fn from_int_write_only_test() {
  permission.from_int(2)
  |> should.equal(PosixPermission(read: False, write: True, execute: False))
}

pub fn from_int_write_execute_test() {
  permission.from_int(3)
  |> should.equal(PosixPermission(read: False, write: True, execute: True))
}

pub fn from_int_read_only_test() {
  permission.from_int(4)
  |> should.equal(PosixPermission(read: True, write: False, execute: False))
}

pub fn from_int_read_execute_test() {
  permission.from_int(5)
  |> should.equal(PosixPermission(read: True, write: False, execute: True))
}

pub fn from_int_read_write_test() {
  permission.from_int(6)
  |> should.equal(PosixPermission(read: True, write: True, execute: False))
}

pub fn from_int_all_permissions_test() {
  permission.from_int(7)
  |> should.equal(PosixPermission(read: True, write: True, execute: True))
}
