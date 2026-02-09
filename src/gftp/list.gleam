//// This module exposes the parser for `LIST`, `MLSD`, and `MLST` commands.
//// 
//// 
//// Please note that there's no guarantee this parser works with `LIST` and the reason is quite simple.
//// There's no specification regarding the LIST command output, so it basically depends on the implementation of the
//// remote FTP server. Despite this though, this parser, has worked on all the ftp server I've used.
//// If you find a variant which doesn't work with this parser,
//// please feel free to report an issue to <https://github.com/veeso/gftp>.
////
//// ## Get started
////
//// Whenever you receive the output for your LIST command, all you have to do is to iterate over lines and
//// call `File::from_str` function as shown in the example.
////
//// ```gleam
//// import gftp/list/file.{type File}
//// import gftp/list.{parse_list}
//// import gleam/result
////
//// // imagine this line received from a LIST command
//// let line = "-rw-r--r-- 1 user group 1234 Nov 5 13:46 example.txt";
////
//// let file = line |> parse_list |> result.unwrap
//// ```

import gftp/list/file.{type File}
import gftp/list/file_type.{type FileType}
import gftp/list/permission.{type FilePermissions, FilePermissions}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/result
import gleam/string
import gleam/time/timestamp.{type Timestamp}
import tempo
import tempo/datetime
import tempo/naive_datetime

const posix_list_regex = "^([\\-ld])([\\-rwxsStT]{9})\\s+(\\d+)\\s+([^ ]+)\\s+([^ ]+)\\s+(\\d+)\\s+([^ ]+\\s+\\d{1,2}\\s+(?:\\d{1,2}:\\d{1,2}|\\d{4}))\\s+(.+)$"

const dos_list_regex = "^(\\d{2}\\-\\d{2}\\-\\d{2}\\s+\\d{2}:\\d{2}\\s*[AP]M)\\s+(<DIR>)?([\\d,]*)\\s+(.+)$"

/// Collapse multiple consecutive whitespace characters into a single space.
fn normalize_whitespace(input: String) -> String {
  let assert Ok(re) = regexp.from_string("\\s+")
  regexp.replace(re, input, " ")
}

/// The result of parsing a line from a `LIST`, `MLSD`, or `MLST` command output.
pub type ParseResult =
  Result(File, ParseError)

/// The possible errors that can occur when parsing a line from a `LIST`, `MLSD`, or `MLST` command output.
pub type ParseError {
  SyntaxError
  InvalidDate
  BadSize
}

/// Describe the given `ParseError` in human-readable form.
pub fn describe_error(error: ParseError) -> String {
  case error {
    SyntaxError -> "the line does not conform to expected format."
    InvalidDate -> "the date field could not be parsed."
    BadSize -> "the size field is not a valid unsigned integer."
  }
}

/// Parse any line from a `MLSD` command output, 
/// returning a `File` data structure with the extracted metadata on success, or a `ParseError` on failure.
pub fn parse_mlsd(line: String) -> ParseResult {
  parse_mlsd_mlst_line(line)
}

/// Parse any line from a `MLST` command output, 
/// returning a `File` data structure with the extracted metadata on success, or a `ParseError` on failure.
pub fn parse_mlst(line: String) -> ParseResult {
  parse_mlsd_mlst_line(line)
}

/// Parse the output of a `LIST` command, which can be in either POSIX or DOS format,
/// returning a `File` data structure with the extracted metadata on success,
/// or a `ParseError` on failure.
/// The parser will first attempt to parse the line as POSIX format,
/// and if that fails, it will attempt to parse it as DOS format.
pub fn parse_list(line: String) -> ParseResult {
  line
  |> parse_list_posix
  |> result.or(parse_list_dos(line))
}

/// Parse MLSD and MLST date formats, which is in the form of `%Y%m%d%H%M%S`
fn parse_mlsd_mlst_time(date_str: String) -> Result(Timestamp, ParseError) {
  let dateformat = tempo.CustomNaive(format: "YYYYMMDDHHmmss")
  date_str
  |> naive_datetime.parse(dateformat)
  |> result.map(naive_datetime.as_utc)
  |> result.map(datetime.to_timestamp)
  |> result.map_error(fn(_) { InvalidDate })
}

/// Parse the permissions string from a `MLSD` or `MLST` line, which is typically a three-digit octal number representing the permissions for the owner, group, and others in a POSIX file system. Each digit can be from 0 to 7, where the bits represent read (4), write (2), and execute (1) permissions.
fn parse_mlsd_mlst_permissions(
  pex_str: String,
) -> Result(FilePermissions, ParseError) {
  let tokens =
    pex_str
    |> string.split("")
    |> list.map(fn(token) { token |> int.base_parse(8) |> result.unwrap(or: 0) })
  case tokens {
    [user, group, other] ->
      Ok(FilePermissions(
        owner: permission.from_int(user),
        group: permission.from_int(group),
        others: permission.from_int(other),
      ))
    _ -> Error(SyntaxError)
  }
}

/// Parse a single token in a `key=value` pair from a `MLSD` or `MLST` line, updating the given `File` data structure with the extracted metadata on success, or returning a `ParseError` on failure.
fn parse_mlsd_mlst_token(key: String, value: String, file: File) -> ParseResult {
  case string.lowercase(key) {
    "type" ->
      case string.lowercase(value) {
        // we can't retrieve the target here
        "file" | "link" -> Ok(file.with_file_type(file, file_type.File))
        "dir" | "cdir" | "pdir" ->
          Ok(file.with_file_type(file, file_type.Directory))
        _ -> Error(SyntaxError)
      }
    "size" ->
      value
      |> int.parse()
      |> result.map(fn(size) { file.with_size(file, size) })
      |> result.map_error(fn(_) { BadSize })
    "modify" ->
      value
      |> parse_mlsd_mlst_time()
      |> result.map(fn(modified) { file.with_modified(file, modified) })
    "unix.uid" ->
      value
      |> int.parse()
      |> result.map(fn(uid) { file.with_uid(file, uid) })
      |> result.map_error(fn(_) { SyntaxError })
    "unix.gid" ->
      value
      |> int.parse()
      |> result.map(fn(uid) { file.with_gid(file, uid) })
      |> result.map_error(fn(_) { SyntaxError })
    "unix.mode" ->
      value
      |> parse_mlsd_mlst_permissions()
      |> result.map(fn(permissions) { file.with_permissions(file, permissions) })
      |> result.map_error(fn(_) { SyntaxError })
    // ignore unknown keys and return the file unchanged
    _ -> Ok(file)
  }
}

/// Parse MLSD and MLST tokens, split by `;`
fn parse_mlsd_mlst_tokens(tokens: List(String), file: File) -> ParseResult {
  case tokens {
    [] -> Ok(file)
    [filename] ->
      parse_mlsd_mlst_tokens([], file.with_name(file, string.trim(filename)))
    [token, ..rest] -> {
      case string.split(token, "=") {
        [key, value] ->
          case parse_mlsd_mlst_token(key, value, file) {
            Ok(updated_file) -> parse_mlsd_mlst_tokens(rest, updated_file)
            Error(error) -> Error(error)
          }
        _ -> Error(SyntaxError)
      }
    }
  }
}

/// Parse any line from a `MLSD` or `MLST` command output,
/// returning a `File` data structure with the extracted metadata on success, or a `ParseError` on failure.
fn parse_mlsd_mlst_line(line: String) -> ParseResult {
  case string.trim(line) {
    "" -> Error(SyntaxError)
    trimmed ->
      trimmed
      |> string.split(";")
      |> parse_mlsd_mlst_tokens(file.empty())
  }
}

/// Parse the file type token from a POSIX LIST output line, which is typically the first character of the line and can be `-` for regular files, `d` for directories, and `l` for symbolic links (although we can't retrieve the target of the symlink in this case).
fn parse_list_file_type(token: String) -> Result(FileType, ParseError) {
  case token {
    "-" -> Ok(file_type.File)
    "d" -> Ok(file_type.Directory)
    // set later
    "l" -> Ok(file_type.Symlink(""))
    _ -> Error(SyntaxError)
  }
}

/// Parse a group permissions set made of three characters (e.g. `rwx`, `r-x`, `rw-`, etc.) from a POSIX LIST output line, where the characters represent read (`r`), write (`w`), and execute (`x`) permissions, and `-` represents no permission. Additionally, `s` and `t` can be used to indicate setuid/setgid and sticky bits, respectively.
fn parse_list_permissions_in_range(
  start: Int,
  token: String,
) -> permission.PosixPermission {
  token
  |> string.slice(start, 3)
  |> string.split("")
  |> list.fold(0, fn(acc, char) {
    case char {
      "r" -> acc + 4
      "w" -> acc + 2
      "x" | "s" | "t" -> acc + 1
      "-" | _ -> acc
    }
  })
  |> permission.from_int
}

/// Parse the permissions string from a POSIX LIST output line, 
/// which is typically a 10-character string where the first character 
/// represents the file type and the next 9 characters represent the 
/// permissions for the owner, group, and others in a POSIX file system. 
/// The permissions are represented as `r` for read, `w` for write, `x` for execute, and `-` for no permission. 
/// Additionally, `s` and `t` can be used to indicate setuid/setgid and sticky bits, respectively.
fn parse_list_permissions(token: String) -> FilePermissions {
  FilePermissions(
    owner: parse_list_permissions_in_range(0, token),
    group: parse_list_permissions_in_range(3, token),
    others: parse_list_permissions_in_range(6, token),
  )
}

/// Parse the last modification time from a POSIX LIST output line, which can be in one of two formats: `MMM D YYYY` (e.g. `Nov 5 2020`)
/// or `MMM D HH:mm` (e.g. `Nov 5 13:46`). The parser will first try to parse the date using the year format, and if that fails, it will try to parse it using the time format.
fn parse_list_lstime(token: String) -> Result(Timestamp, ParseError) {
  let normalized =
    token
    |> string.trim()
    |> normalize_whitespace()

  // Try year format first: "Nov 5 2020" -> "Nov 5 2020 00:00" (add default time)
  let fmt_year = tempo.CustomNaive(format: "MMM D YYYY HH:mm")
  let year_result =
    { normalized <> " 00:00" }
    |> naive_datetime.parse(fmt_year)
    |> result.map(naive_datetime.as_utc)
    |> result.map(datetime.to_timestamp)

  // Try time format: "Nov 5 13:46" -> "1970 Nov 5 13:46" (add default year)
  let fmt_time = tempo.CustomNaive(format: "YYYY MMM D HH:mm")
  let time_result =
    { "1970 " <> normalized }
    |> naive_datetime.parse(fmt_time)
    |> result.map(naive_datetime.as_utc)
    |> result.map(datetime.to_timestamp)

  year_result
  |> result.or(time_result)
  |> result.map_error(fn(_) { InvalidDate })
}

/// Parse the file name and symlink target (if applicable) from a POSIX LIST output line, which is typically the last token of the line.
/// If the file type is a symbolic link, the token may contain the file name followed by `->` and the target of the symlink (e.g. `symlink -> target`).
fn parse_list_name_and_link(
  file_type: file_type.FileType,
  token: String,
) -> Result(#(String, Option(String)), ParseError) {
  let get_name_and_link = fn(parts) {
    case parts {
      [name, target] -> Ok(#(name, Some(target)))
      [name] -> Ok(#(name, None))
      _ -> Error(SyntaxError)
    }
  }

  case file_type {
    file_type.Symlink(_) ->
      token
      |> string.split(" -> ")
      |> get_name_and_link
    _ -> Ok(#(token, None))
  }
}

/// A helper function to match a regular expression against a string and extract the submatches as a list of strings.
fn re_matches(
  re: regexp.Regexp,
  line: String,
) -> Result(List(Option(String)), ParseError) {
  case regexp.scan(re, line) {
    [] -> Error(SyntaxError)
    matches ->
      matches
      |> list.map(fn(match) { match.submatches })
      |> list.flatten()
      |> Ok
  }
}

/// Parse a POSIX LIST output line and if it is valid, return a [`File`] instance, otherwise return a [`ParseError`].
///
/// POSIX syntax has the following syntax:
///
/// ```text
/// {FILE_TYPE}{PERMISSIONS} {LINK_COUNT} {USER} {GROUP} {FILE_SIZE} {MODIFIED_TIME} {FILENAME}
/// -rw-r--r-- 1 user group 1234 Nov 5 13:46 example.txt
/// ```
fn parse_list_posix(line: String) -> ParseResult {
  let assert Ok(re) = regexp.from_string(posix_list_regex)

  case re_matches(re, line) {
    Ok([
      Some(file_type),
      Some(permissions),
      _link_count,
      Some(user),
      Some(group),
      Some(size),
      Some(modified),
      Some(name),
    ]) -> {
      use file_type <- result.try(parse_list_file_type(file_type))
      use size <- result.try(
        size
        |> string.trim()
        |> int.parse()
        |> result.replace_error(BadSize),
      )
      let permissions = parse_list_permissions(permissions)
      use modified <- result.try(
        modified
        |> parse_list_lstime()
        |> result.replace_error(InvalidDate),
      )
      let uid =
        user
        |> string.trim()
        |> int.parse()
        |> option.from_result()
      let gid =
        group
        |> string.trim()
        |> int.parse()
        |> option.from_result()
      use #(name, symlink_path) <- result.try(parse_list_name_and_link(
        file_type,
        name,
      ))

      let resolved_file_type = fn(
        file_type: file_type.FileType,
        symlink_path: Option(String),
      ) {
        case file_type {
          file_type.Symlink(_) ->
            case symlink_path {
              Some(target) -> file_type.Symlink(target)
              None -> file_type.File
            }
          _ -> file_type
        }
      }

      let f =
        file.empty()
        |> file.with_name(name)
        |> file.with_file_type(resolved_file_type(file_type, symlink_path))
        |> file.with_size(size)
        |> file.with_modified(modified)
        |> file.with_permissions(permissions)

      let f = case uid {
        Some(uid) -> file.with_uid(f, uid)
        None -> f
      }
      let f = case gid {
        Some(gid) -> file.with_gid(f, gid)
        None -> f
      }
      Ok(f)
    }
    _ -> Error(SyntaxError)
  }
}

/// Parse a date and time in the format used by DOS LIST output, which is typically `MM-DD-YY hh:mmA` (e.g. `10-19-20 03:19PM`).
fn parse_list_dos_time(token: String) -> Result(Timestamp, ParseError) {
  let fmt = tempo.CustomNaive(format: "MM-DD-YY hh:mmA")
  token
  |> string.trim()
  |> normalize_whitespace()
  |> naive_datetime.parse(fmt)
  |> result.map(naive_datetime.as_utc)
  |> result.map(datetime.to_timestamp)
  |> result.map_error(fn(_) { InvalidDate })
}

/// Try to parse a "LIST" output command line in DOS format.
/// Returns [`ParseError`] if syntax is not DOS compliant.
/// DOS syntax has the following syntax:
///
/// ```text
/// {DATE} {TIME} {<DIR> | SIZE} {FILENAME}
/// 10-19-20  03:19PM <DIR> pub
/// 04-08-14  03:09PM 403   readme.txt
/// ```
fn parse_list_dos(line: String) -> ParseResult {
  let assert Ok(re) = regexp.from_string(dos_list_regex)

  case re_matches(re, line) {
    Ok([Some(modified), file_type, size, Some(name)]) -> {
      use modified <- result.try(
        modified
        |> parse_list_dos_time()
        |> result.replace_error(InvalidDate),
      )

      let file_type = case option.is_some(file_type) {
        True -> file_type.Directory
        False -> file_type.File
      }

      let size = case size {
        None -> 0
        Some(size_str) ->
          size_str
          |> string.trim()
          |> string.replace(",", "")
          |> int.parse()
          |> result.map_error(fn(_) { BadSize })
          |> result.unwrap(or: 0)
      }

      file.empty()
      |> file.with_name(name)
      |> file.with_file_type(file_type)
      |> file.with_size(size)
      |> file.with_modified(modified)
      |> Ok
    }
    _ -> Error(SyntaxError)
  }
}
