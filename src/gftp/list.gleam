//// Parsers for FTP directory listing formats: `LIST` (POSIX and DOS), `MLSD`, and `MLST`.
////
//// ## LIST parsing
////
//// The `LIST` output format is not standardized and depends on the FTP server.
//// This parser supports both POSIX and DOS formats, trying POSIX first.
//// If you find a format that doesn't parse correctly, please report an issue
//// at <https://github.com/veeso/gftp>.
////
//// ```gleam
//// import gftp/list
//// import gftp/list/file
////
//// let line = "-rw-r--r-- 1 user group 1234 Nov 5 13:46 example.txt"
//// let assert Ok(f) = list.parse_list(line)
//// let name = file.name(f) // "example.txt"
//// let size = file.size(f) // 1234
//// ```
////
//// ## MLSD/MLST parsing (RFC 3659)
////
//// Machine-readable listing formats provide structured, standardized output:
////
//// ```gleam
//// import gftp/list
//// import gftp/list/file
////
//// let line = "type=file;size=1234;modify=20200105134600; example.txt"
//// let assert Ok(f) = list.parse_mlsd(line)
//// let name = file.name(f) // "example.txt"
//// ```
////
//// ## Usage with gftp
////
//// ```gleam
//// import gftp
//// import gftp/list
//// import gleam/list as gleam_list
//// import gleam/option.{None}
////
//// let assert Ok(lines) = gftp.list(client, None)
//// let assert Ok(files) = gleam_list.try_map(lines, list.parse_list)
//// ```

import gftp/list/file.{type File}
import gftp/list/file_type.{type FileType}
import gftp/list/permission.{type FilePermissions, FilePermissions}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/regexp
import gleam/result
import gleam/string
import gleam/time/duration
import gleam/time/timestamp.{type Timestamp}
import tempo
import tempo/datetime
import tempo/naive_datetime

const posix_list_regex = "^([\\-ld])([\\-rwxsStT]{9})\\s+(\\d+)\\s+([^ ]+)\\s+([^ ]+)\\s+(\\d+)\\s+([^ ]+\\s+\\d{1,2}\\s+(?:\\d{1,2}:\\d{1,2}|\\d{4}))\\s+(.+)$"

const dos_list_regex = "^(\\d{2}\\-\\d{2}\\-\\d{2}\\s+\\d{2}:\\d{2}\\s*[AP]M)\\s+(<DIR>)?([\\d,]*)\\s+(.+)$"

/// Collapse multiple consecutive whitespace characters into a single space.
fn normalize_whitespace(input: String) -> String {
  case regexp.from_string("\\s+") {
    Ok(re) -> regexp.replace(re, input, " ")
    // The regex is a compile-time constant and always valid
    Error(_) -> input
  }
}

/// Match a regular expression against a string and extract submatches.
fn re_matches(
  re: regexp.Regexp,
  line: String,
) -> Result(List(Option(String)), Nil) {
  case regexp.scan(re, line) {
    [] -> Error(Nil)
    matches ->
      matches
      |> list.map(fn(match) { match.submatches })
      |> list.flatten()
      |> Ok
  }
}

/// The possible errors that can occur when parsing a line from a `LIST`, `MLSD`, or `MLST` command output.
pub type ParseError {
  /// The line does not conform to expected LIST, MLSD, or MLST format.
  SyntaxError
  /// The date field could not be parsed.
  InvalidDate
  /// The size field is not a valid unsigned integer.
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

/// Parse a line from `MLSD` command output into a `File`.
///
/// ```gleam
/// let line = "type=file;size=1024;modify=20200105134600; readme.txt"
/// let assert Ok(f) = list.parse_mlsd(line)
/// ```
pub fn parse_mlsd(line: String) -> Result(File, ParseError) {
  parse_mlsd_mlst_line(line)
}

/// Parse a line from `MLST` command output into a `File`.
///
/// ```gleam
/// let line = "type=file;size=2048;modify=20210315120000; document.pdf"
/// let assert Ok(f) = list.parse_mlst(line)
/// ```
pub fn parse_mlst(line: String) -> Result(File, ParseError) {
  parse_mlsd_mlst_line(line)
}

/// Parse a `LIST` output line (POSIX or DOS format) into a `File`.
///
/// Tries POSIX format first, then falls back to DOS format.
///
/// ```gleam
/// // POSIX format
/// let assert Ok(f) = list.parse_list("-rw-r--r-- 1 user group 1234 Nov 5 13:46 example.txt")
///
/// // DOS format
/// let assert Ok(f) = list.parse_list("10-19-20  03:19PM       403 readme.txt")
/// ```
pub fn parse_list(line: String) -> Result(File, ParseError) {
  // Both regexes are compile-time constants and always valid
  use posix_re <- result.try(
    regexp.from_string(posix_list_regex)
    |> result.replace_error(SyntaxError),
  )
  use dos_re <- result.try(
    regexp.from_string(dos_list_regex)
    |> result.replace_error(SyntaxError),
  )

  line
  |> parse_list_posix(posix_re)
  |> result.or(parse_list_dos(line, dos_re))
}

/// Parse MLSD and MLST date formats, which is in the form of `%Y%m%d%H%M%S`
fn parse_mlsd_mlst_time(date_str: String) -> Result(Timestamp, ParseError) {
  parse_datetime_with_format(date_str, "YYYYMMDDHHmmss")
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
    // support both three-digit (user, group, other) and four-digit (setuid/setgid/ sticky + user, group, other) formats,
    // ignoring the setuid/setgid/sticky bits if present (e.g. `755` or `0755` both parse to the same permissions).
    [user, group, other] | [_, user, group, other] ->
      Ok(FilePermissions(
        owner: permission.from_int(user),
        group: permission.from_int(group),
        others: permission.from_int(other),
      ))
    _ -> Error(SyntaxError)
  }
}

/// Parse a single token in a `key=value` pair from a `MLSD` or `MLST` line, updating the given `File` data structure with the extracted metadata on success, or returning a `ParseError` on failure.
fn parse_mlsd_mlst_token(
  key: String,
  value: String,
  file: File,
) -> Result(File, ParseError) {
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
fn parse_mlsd_mlst_tokens(
  tokens: List(String),
  file: File,
) -> Result(File, ParseError) {
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
fn parse_mlsd_mlst_line(line: String) -> Result(File, ParseError) {
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
  let year_result =
    { normalized <> " 00:00" }
    |> parse_datetime_with_format("MMM D YYYY HH:mm")

  // Try time format: "Nov 5 13:46" -> "2026 Nov 5 13:46" (assume current year).
  // If the resulting date is more than 6 months in the future, it likely refers
  // to the previous year (matching GNU ls behavior). For example, parsing
  // "Dec 25 14:30" in January should resolve to December of the previous year.
  let now = timestamp.system_time()
  let current_year =
    now
    |> timestamp.to_calendar(duration.seconds(0))
    |> fn(res) { { res.0 }.year }
  let time_result =
    { int.to_string(current_year) <> " " <> normalized }
    |> parse_datetime_with_format("YYYY MMM D HH:mm")
    |> result.try(fn(parsed) {
      adjust_year_if_future(parsed, now, current_year, normalized)
    })

  year_result
  |> result.or(time_result)
  |> result.map_error(fn(_) { InvalidDate })
}

/// If `parsed` is more than ~6 months in the future relative to `now`,
/// re-parse using the previous year.
fn adjust_year_if_future(
  parsed: Timestamp,
  now: Timestamp,
  current_year: Int,
  normalized: String,
) -> Result(Timestamp, ParseError) {
  let six_months_in_seconds = 180 * 24 * 3600
  let diff = timestamp.difference(parsed, now)
  case duration.compare(diff, duration.seconds(six_months_in_seconds)) {
    order.Gt -> {
      { int.to_string(current_year - 1) <> " " <> normalized }
      |> parse_datetime_with_format("YYYY MMM D HH:mm")
    }
    _ -> Ok(parsed)
  }
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

/// Resolve the file type for a POSIX LIST entry, filling in the symlink target
/// when both the type indicator is `l` and the name contained ` -> target`.
fn resolve_posix_file_type(
  ft: FileType,
  symlink_path: Option(String),
) -> FileType {
  case ft, symlink_path {
    file_type.Symlink(_), Some(target) -> file_type.Symlink(target)
    file_type.Symlink(_), None -> file_type.File
    _, _ -> ft
  }
}

/// Optionally set a field on a File when the value is Some.
fn maybe_set(f: File, value: Option(a), setter: fn(File, a) -> File) -> File {
  case value {
    Some(v) -> setter(f, v)
    None -> f
  }
}

/// Try to parse a string as an Int, returning None on failure.
fn try_parse_int(s: String) -> Option(Int) {
  s |> string.trim() |> int.parse() |> option.from_result()
}

/// Parse a POSIX LIST output line and if it is valid, return a [`File`] instance, otherwise return a [`ParseError`].
///
/// POSIX syntax has the following syntax:
///
/// ```text
/// {FILE_TYPE}{PERMISSIONS} {LINK_COUNT} {USER} {GROUP} {FILE_SIZE} {MODIFIED_TIME} {FILENAME}
/// -rw-r--r-- 1 user group 1234 Nov 5 13:46 example.txt
/// ```
fn parse_list_posix(line: String, re: regexp.Regexp) -> Result(File, ParseError) {
  case re_matches(re, line) {
    Ok([
      Some(ft_str),
      Some(perm_str),
      _link_count,
      Some(user),
      Some(group),
      Some(size_str),
      Some(modified_str),
      Some(name_str),
    ]) -> {
      use ft <- result.try(parse_list_file_type(ft_str))
      use size <- result.try(
        size_str
        |> string.trim()
        |> int.parse()
        |> result.replace_error(BadSize),
      )
      use modified <- result.try(
        modified_str
        |> parse_list_lstime()
        |> result.replace_error(InvalidDate),
      )
      use #(name, symlink_path) <- result.try(parse_list_name_and_link(
        ft,
        name_str,
      ))

      file.empty()
      |> file.with_name(name)
      |> file.with_file_type(resolve_posix_file_type(ft, symlink_path))
      |> file.with_size(size)
      |> file.with_modified(modified)
      |> file.with_permissions(parse_list_permissions(perm_str))
      |> maybe_set(try_parse_int(user), file.with_uid)
      |> maybe_set(try_parse_int(group), file.with_gid)
      |> Ok
    }
    _ -> Error(SyntaxError)
  }
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
fn parse_list_dos(line: String, re: regexp.Regexp) -> Result(File, ParseError) {
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

      use size <- result.try(case size {
        None -> Ok(0)
        Some(size_str) ->
          size_str
          |> string.trim()
          |> string.replace(",", "")
          |> int.parse()
          |> result.replace_error(BadSize)
      })

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

/// Parse a date and time in the format used by DOS LIST output, 
/// which is typically `MM-DD-YY hh:mmA` (e.g. `10-19-20 03:19PM`), or with a space before the AM/PM (e.g. `10-19-20 03:19 PM`).
fn parse_list_dos_time(token: String) -> Result(Timestamp, ParseError) {
  token
  |> parse_datetime_with_format("MM-DD-YY hh:mmA")
  |> result.or(parse_datetime_with_format(token, "MM-DD-YY hh:mm A"))
}

/// Try to a string as a datetime in the provided format, returning a Timestamp on success or a ParseError on failure.
fn parse_datetime_with_format(
  s: String,
  format: String,
) -> Result(Timestamp, ParseError) {
  let fmt = tempo.CustomNaive(format: format)
  s
  |> string.trim()
  |> normalize_whitespace()
  |> naive_datetime.parse(fmt)
  |> result.map(naive_datetime.as_utc)
  |> result.map(datetime.to_timestamp)
  |> result.map_error(fn(_) { InvalidDate })
}
