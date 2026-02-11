import gftp/list/file
import gftp/list/file_type
import gftp/list/permission.{FilePermissions, PosixPermission}
import gleam/option.{None, Some}
import gleam/time/timestamp

pub fn empty_file_name_test() {
  let assert "" =
    file.empty()
    |> file.name
}

pub fn empty_file_type_test() {
  let assert file_type.File =
    file.empty()
    |> file.file_type
}

pub fn empty_file_size_test() {
  let assert 0 =
    file.empty()
    |> file.size
}

pub fn empty_file_modified_test() {
  let assert None =
    file.empty()
    |> file.modified
}

pub fn empty_file_uid_test() {
  let assert None =
    file.empty()
    |> file.uid
}

pub fn empty_file_gid_test() {
  let assert None =
    file.empty()
    |> file.gid
}

pub fn empty_file_permissions_test() {
  let assert None =
    file.empty()
    |> file.permissions
}

pub fn empty_file_symlink_target_test() {
  let assert None =
    file.empty()
    |> file.symlink_target
}

pub fn with_name_test() {
  let assert "test.txt" =
    file.empty()
    |> file.with_name("test.txt")
    |> file.name
}

pub fn with_file_type_directory_test() {
  let assert file_type.Directory =
    file.empty()
    |> file.with_file_type(file_type.Directory)
    |> file.file_type
}

pub fn with_file_type_symlink_test() {
  let f =
    file.empty()
    |> file.with_file_type(file_type.Symlink("/target"))
  let assert file_type.Symlink("/target") =
    f
    |> file.file_type
  let assert Some("/target") =
    f
    |> file.symlink_target
}

pub fn with_size_test() {
  let assert 4096 =
    file.empty()
    |> file.with_size(4096)
    |> file.size
}

pub fn with_modified_test() {
  let ts = timestamp.from_unix_seconds(1_604_583_960)
  let assert Some(_) =
    file.empty()
    |> file.with_modified(ts)
    |> file.modified
}

pub fn with_permissions_test() {
  let perms =
    FilePermissions(
      owner: PosixPermission(read: True, write: True, execute: False),
      group: PosixPermission(read: True, write: False, execute: False),
      others: PosixPermission(read: True, write: False, execute: False),
    )
  let assert Some(_) =
    file.empty()
    |> file.with_permissions(perms)
    |> file.permissions
}

pub fn with_uid_test() {
  let assert Some(1000) =
    file.empty()
    |> file.with_uid(1000)
    |> file.uid
}

pub fn with_gid_test() {
  let assert Some(1000) =
    file.empty()
    |> file.with_gid(1000)
    |> file.gid
}

pub fn symlink_target_on_non_symlink_test() {
  let assert None =
    file.empty()
    |> file.with_file_type(file_type.File)
    |> file.symlink_target
}

pub fn symlink_target_on_directory_test() {
  let assert None =
    file.empty()
    |> file.with_file_type(file_type.Directory)
    |> file.symlink_target
}
