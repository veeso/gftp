import gftp/list/file_type
import gleam/option.{None, Some}

pub fn is_file_with_file_test() {
  let assert True =
    file_type.File
    |> file_type.is_file
}

pub fn is_file_with_directory_test() {
  let assert False =
    file_type.Directory
    |> file_type.is_file
}

pub fn is_file_with_symlink_test() {
  let assert False =
    file_type.Symlink("/target")
    |> file_type.is_file
}

pub fn is_directory_with_directory_test() {
  let assert True =
    file_type.Directory
    |> file_type.is_directory
}

pub fn is_directory_with_file_test() {
  let assert False =
    file_type.File
    |> file_type.is_directory
}

pub fn is_directory_with_symlink_test() {
  let assert False =
    file_type.Symlink("/target")
    |> file_type.is_directory
}

pub fn is_symlink_with_symlink_test() {
  let assert True =
    file_type.Symlink("/target")
    |> file_type.is_symlink
}

pub fn is_symlink_with_file_test() {
  let assert False =
    file_type.File
    |> file_type.is_symlink
}

pub fn is_symlink_with_directory_test() {
  let assert False =
    file_type.Directory
    |> file_type.is_symlink
}

pub fn symlink_target_with_symlink_test() {
  let assert Some("/usr/bin/python") =
    file_type.Symlink("/usr/bin/python")
    |> file_type.symlink
}

pub fn symlink_target_with_file_test() {
  let assert None =
    file_type.File
    |> file_type.symlink
}

pub fn symlink_target_with_directory_test() {
  let assert None =
    file_type.Directory
    |> file_type.symlink
}
