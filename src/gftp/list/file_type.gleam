//// This module contains the FileType type which provides information about the type of a file,
//// such as whether it is a directory, a symbolic link, or a regular file.

import gleam/option.{type Option, None, Some}

/// The `FileType` type represents the type of a file, which can be a regular file, a directory, or a symbolic link.
pub type FileType {
  /// A regular file.
  File
  /// A directory.
  Directory
  /// A symbolic link, which points to another file or directory.
  Symlink(to: String)
}

/// Returns `True` if the file type is a regular file, and `False` otherwise.
pub fn is_file(file_type: FileType) -> Bool {
  case file_type {
    File -> True
    _ -> False
  }
}

/// Returns `True` if the file type is a directory, and `False` otherwise.
pub fn is_directory(file_type: FileType) -> Bool {
  case file_type {
    Directory -> True
    _ -> False
  }
}

/// Returns `True` if the file type is a symbolic link, and `False` otherwise.
pub fn is_symlink(file_type: FileType) -> Bool {
  case file_type {
    Symlink(_) -> True
    _ -> False
  }
}

/// If the file type is a symbolic link, returns the target of the link. Otherwise, returns `None`.
pub fn symlink(file_type: FileType) -> Option(String) {
  case file_type {
    Symlink(to) -> Some(to)
    _ -> None
  }
}
