import gftp/list/file_type
import gleam/option.{None, Some}
import gleeunit/should

pub fn is_file_with_file_test() {
  file_type.File
  |> file_type.is_file
  |> should.be_true
}

pub fn is_file_with_directory_test() {
  file_type.Directory
  |> file_type.is_file
  |> should.be_false
}

pub fn is_file_with_symlink_test() {
  file_type.Symlink("/target")
  |> file_type.is_file
  |> should.be_false
}

pub fn is_directory_with_directory_test() {
  file_type.Directory
  |> file_type.is_directory
  |> should.be_true
}

pub fn is_directory_with_file_test() {
  file_type.File
  |> file_type.is_directory
  |> should.be_false
}

pub fn is_directory_with_symlink_test() {
  file_type.Symlink("/target")
  |> file_type.is_directory
  |> should.be_false
}

pub fn is_symlink_with_symlink_test() {
  file_type.Symlink("/target")
  |> file_type.is_symlink
  |> should.be_true
}

pub fn is_symlink_with_file_test() {
  file_type.File
  |> file_type.is_symlink
  |> should.be_false
}

pub fn is_symlink_with_directory_test() {
  file_type.Directory
  |> file_type.is_symlink
  |> should.be_false
}

pub fn symlink_target_with_symlink_test() {
  file_type.Symlink("/usr/bin/python")
  |> file_type.symlink
  |> should.equal(Some("/usr/bin/python"))
}

pub fn symlink_target_with_file_test() {
  file_type.File
  |> file_type.symlink
  |> should.equal(None)
}

pub fn symlink_target_with_directory_test() {
  file_type.Directory
  |> file_type.symlink
  |> should.equal(None)
}
