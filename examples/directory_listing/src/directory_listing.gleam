import envoy
import gftp
import gftp/list as gftp_list
import gftp/list/file
import gftp/list/file_type
import gftp/list/permission.{type PosixPermission}
import gftp/result as ftp_result
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleam/time/calendar
import gleam/time/duration
import gleam/time/timestamp.{type Timestamp}

pub fn main() {
  // Read connection details from environment variables
  let assert Ok(host) = envoy.get("FTP_HOST")
  let port = envoy.get("FTP_PORT") |> result.try(int.parse) |> result.unwrap(21)
  let assert Ok(user) = envoy.get("FTP_USER")
  let assert Ok(password) = envoy.get("FTP_PASSWORD")

  io.println("Connecting to " <> host <> ":" <> int.to_string(port) <> "...")

  // Connect and login
  let assert Ok(client) = gftp.connect_timeout(host, port, timeout: 10_000)
  let assert Ok(_) = gftp.login(client, user, password)
  io.println("Logged in as " <> user <> ".\n")

  // LIST command with parsed output
  io.println("=== LIST (parsed) ===")
  let assert Ok(lines) = gftp.list(client, None)
  let files =
    list.filter_map(lines, fn(line) {
      case gftp_list.parse_list(line) {
        Ok(f) -> Ok(f)
        Error(e) -> {
          io.println(
            "  [parse error: " <> gftp_list.describe_error(e) <> "] " <> line,
          )
          Error(Nil)
        }
      }
    })
  list.each(files, fn(f) { io.println(format_file(f)) })
  io.println(
    "\nTotal: " <> int.to_string(list.length(files)) <> " entries parsed.\n",
  )

  // Try MLSD if the server supports it
  io.println("=== MLSD (parsed) ===")
  case gftp.mlsd(client, None) {
    Ok(mlsd_lines) -> {
      let mlsd_files =
        list.filter_map(mlsd_lines, fn(line) {
          case gftp_list.parse_mlsd(line) {
            Ok(f) -> Ok(f)
            Error(e) -> {
              io.println(
                "  [parse error: "
                <> gftp_list.describe_error(e)
                <> "] "
                <> line,
              )
              Error(Nil)
            }
          }
        })
      list.each(mlsd_files, fn(f) { io.println(format_file(f)) })
      io.println(
        "\nTotal: "
        <> int.to_string(list.length(mlsd_files))
        <> " entries parsed.",
      )
    }
    Error(err) ->
      io.println(
        "MLSD not supported by this server: " <> ftp_result.describe_error(err),
      )
  }

  // Disconnect
  let assert Ok(_) = gftp.quit(client)
  let _ = gftp.shutdown(client)
  io.println("\nDisconnected. Done!")
}

fn format_file(f: file.File) -> String {
  let type_str = case file.file_type(f) {
    file_type.File -> "FILE"
    file_type.Directory -> "DIR "
    file_type.Symlink(to) -> "LINK -> " <> to
  }

  let size_str = int.to_string(file.size(f)) <> " bytes"

  let modified_str = case file.modified(f) {
    Some(ts) -> format_timestamp(ts)
    None -> "(no date)"
  }

  let perm_str = case file.permissions(f) {
    Some(perms) -> format_permissions(perms)
    None -> ""
  }

  "  "
  <> pad_right(type_str, 12)
  <> pad_right(perm_str, 12)
  <> pad_right(size_str, 16)
  <> pad_right(modified_str, 22)
  <> file.name(f)
}

fn format_timestamp(ts: Timestamp) -> String {
  let #(date, time) = timestamp.to_calendar(ts, duration.seconds(0))

  int.to_string(date.year)
  <> "-"
  <> pad_int(month_to_int(date.month))
  <> "-"
  <> pad_int(date.day)
  <> " "
  <> pad_int(time.hours)
  <> ":"
  <> pad_int(time.minutes)
}

fn format_permissions(perms: permission.FilePermissions) -> String {
  format_perm(perms.owner)
  <> format_perm(perms.group)
  <> format_perm(perms.others)
}

fn format_perm(p: PosixPermission) -> String {
  case p.read {
    True -> "r"
    False -> "-"
  }
  <> case p.write {
    True -> "w"
    False -> "-"
  }
  <> case p.execute {
    True -> "x"
    False -> "-"
  }
}

fn pad_right(s: String, width: Int) -> String {
  let len = string.length(s)
  case len >= width {
    True -> s
    False -> s <> string.repeat(" ", width - len)
  }
}

fn pad_int(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}

fn month_to_int(month: calendar.Month) -> Int {
  case month {
    calendar.January -> 1
    calendar.February -> 2
    calendar.March -> 3
    calendar.April -> 4
    calendar.May -> 5
    calendar.June -> 6
    calendar.July -> 7
    calendar.August -> 8
    calendar.September -> 9
    calendar.October -> 10
    calendar.November -> 11
    calendar.December -> 12
  }
}
