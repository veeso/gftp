import gftp/list/file
import gftp/list/file_type
import gftp/list/permission.{FilePermissions, PosixPermission}
import gleam/option.{None, Some}
import gleam/time/timestamp
import gleeunit/should

pub fn empty_file_name_test() {
  file.empty()
  |> file.name
  |> should.equal("")
}

pub fn empty_file_type_test() {
  file.empty()
  |> file.file_type
  |> should.equal(file_type.File)
}

pub fn empty_file_size_test() {
  file.empty()
  |> file.size
  |> should.equal(0)
}

pub fn empty_file_modified_test() {
  file.empty()
  |> file.modified
  |> should.equal(None)
}

pub fn empty_file_uid_test() {
  file.empty()
  |> file.uid
  |> should.equal(None)
}

pub fn empty_file_gid_test() {
  file.empty()
  |> file.gid
  |> should.equal(None)
}

pub fn empty_file_permissions_test() {
  file.empty()
  |> file.permissions
  |> should.equal(None)
}

pub fn empty_file_symlink_target_test() {
  file.empty()
  |> file.symlink_target
  |> should.equal(None)
}

pub fn with_name_test() {
  file.empty()
  |> file.with_name("test.txt")
  |> file.name
  |> should.equal("test.txt")
}

pub fn with_file_type_directory_test() {
  file.empty()
  |> file.with_file_type(file_type.Directory)
  |> file.file_type
  |> should.equal(file_type.Directory)
}

pub fn with_file_type_symlink_test() {
  let f =
    file.empty()
    |> file.with_file_type(file_type.Symlink("/target"))
  f
  |> file.file_type
  |> should.equal(file_type.Symlink("/target"))
  f
  |> file.symlink_target
  |> should.equal(Some("/target"))
}

pub fn with_size_test() {
  file.empty()
  |> file.with_size(4096)
  |> file.size
  |> should.equal(4096)
}

pub fn with_modified_test() {
  let ts = timestamp.from_unix_seconds(1_604_583_960)
  file.empty()
  |> file.with_modified(ts)
  |> file.modified
  |> should.equal(Some(ts))
}

pub fn with_permissions_test() {
  let perms =
    FilePermissions(
      owner: PosixPermission(read: True, write: True, execute: False),
      group: PosixPermission(read: True, write: False, execute: False),
      others: PosixPermission(read: True, write: False, execute: False),
    )
  file.empty()
  |> file.with_permissions(perms)
  |> file.permissions
  |> should.equal(Some(perms))
}

pub fn with_uid_test() {
  file.empty()
  |> file.with_uid(1000)
  |> file.uid
  |> should.equal(Some(1000))
}

pub fn with_gid_test() {
  file.empty()
  |> file.with_gid(1000)
  |> file.gid
  |> should.equal(Some(1000))
}

pub fn symlink_target_on_non_symlink_test() {
  file.empty()
  |> file.with_file_type(file_type.File)
  |> file.symlink_target
  |> should.equal(None)
}

pub fn symlink_target_on_directory_test() {
  file.empty()
  |> file.with_file_type(file_type.Directory)
  |> file.symlink_target
  |> should.equal(None)
}
