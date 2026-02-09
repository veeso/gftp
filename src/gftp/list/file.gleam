//// File type returned by the `list` functions, representing a file or directory in the FTP server's file system.

import gftp/list/file_type.{type FileType}
import gftp/list/permission.{type FilePermissions}
import gleam/option.{type Option, None, Some}
import gleam/time/timestamp.{type Timestamp}

/// Describes a file entry on the remote system.
/// This data type is returned in a collection after parsing a LIST output
///
/// Each file comes with metadata such as name, type (file, directory, symlink),
/// size, modification time, POSIX permissions and owner/group ids (if available).
///
/// # Parsing
///
/// You can parse a LIST, MLSD, or MLST line by using the `gftp/list` module by calling any of the following functions:
///
/// - `parse_list` - tries to parse the line as either POSIX or DOS format, returning an error if both fail
/// - `parse_mlsd` - parses the line as MLSD format, which is a standardized format for machine-readable directory listings (RFC 3659)
/// - `parse_mlst` - parses the line as MLST format, which is a standardized format for machine-readable file listings (RFC 3659)
///
/// In case you opt for `parse_list`, the parser will first try to parse the line
/// as POSIX format and if it fails, it will try to parse it as DOS format.
pub opaque type File {
  File(
    name: String,
    file_type: FileType,
    size: Int,
    modified: Option(Timestamp),
    uid: Option(Int),
    gid: Option(Int),
    permissions: Option(FilePermissions),
  )
}

/// Get the name of the file or directory.
pub fn name(file: File) -> String {
  file.name
}

/// Get the type of the file or directory.
pub fn file_type(file: File) -> FileType {
  file.file_type
}

/// Get the size of the file in bytes. For directories, this may be implementation-defined and not necessarily meaningful.
pub fn size(file: File) -> Int {
  file.size
}

/// Get the last modification time of the file or directory as a `Timestamp`.
pub fn modified(file: File) -> Option(Timestamp) {
  file.modified
}

/// Get the user ID (UID) of the file owner, if available. 
/// This is typically a POSIX-specific attribute and may not be present in all FTP server implementations.
pub fn uid(file: File) -> Option(Int) {
  file.uid
}

/// Get the group ID (GID) of the file owner, if available. 
/// This is typically a POSIX-specific attribute and may not be present in all FTP server implementations.
pub fn gid(file: File) -> Option(Int) {
  file.gid
}

/// Get the POSIX permissions of the file or directory, if available.
/// This is typically a POSIX-specific attribute and may not be present in all FTP server implementations
pub fn permissions(file: File) -> Option(FilePermissions) {
  file.permissions
}

/// If the file is a symbolic link, get the target path of the link. Otherwise, return `None`.
pub fn symlink_target(file: File) -> Option(String) {
  case file.file_type {
    file_type.Symlink(target) -> Some(target)
    _ -> None
  }
}

/// Create a new empty `File` with the given name and default values for other fields.
pub fn empty() -> File {
  File("", file_type.File, 0, None, None, None, None)
}

/// Set the name of the file or directory and return a new `File` with the updated name.
pub fn with_name(file: File, name: String) -> File {
  File(..file, name: name)
}

/// Set the type of the file (file, directory, or symlink) and return a new `File` with the updated type.
pub fn with_file_type(file: File, file_type: FileType) -> File {
  File(..file, file_type: file_type)
}

/// Set the size of the file in bytes and return a new `File` with the updated size.
pub fn with_size(file: File, size: Int) -> File {
  File(..file, size: size)
}

/// Set the last modification time of the file or directory, if available. This is typically a POSIX-specific operation and may not be supported by all FTP servers.
pub fn with_modified(file: File, modified: Timestamp) -> File {
  File(..file, modified: Some(modified))
}

/// Set the POSIX permissions of the file or directory, if available. This is typically a POSIX-specific operation and may not be supported by all FTP servers.
pub fn with_permissions(file: File, permissions: FilePermissions) -> File {
  File(..file, permissions: Some(permissions))
}

/// Set the UID of the file, if available. This is typically a POSIX-specific operation and may not be supported by all FTP servers.
pub fn with_uid(file: File, uid: Int) -> File {
  File(..file, uid: Some(uid))
}

/// Set the GID of the file, if available. This is typically a POSIX-specific operation and may not be supported by all FTP servers.
pub fn with_gid(file: File, gid: Int) -> File {
  File(..file, gid: Some(gid))
}
