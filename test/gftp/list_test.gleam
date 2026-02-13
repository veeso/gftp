import gftp/list
import gftp/list/file
import gftp/list/file_type
import gftp/list/permission.{FilePermissions, PosixPermission}
import gleam/option.{None, Some}
import gleam/time/calendar
import gleam/time/duration
import gleam/time/timestamp
import tempo
import tempo/date as tempo_date

// -- describe_error tests --

pub fn describe_error_syntax_error_test() {
  let assert "the line does not conform to expected format." =
    list.describe_error(list.SyntaxError)
}

pub fn describe_error_invalid_date_test() {
  let assert "the date field could not be parsed." =
    list.describe_error(list.InvalidDate)
}

pub fn describe_error_bad_size_test() {
  let assert "the size field is not a valid unsigned integer." =
    list.describe_error(list.BadSize)
}

// -- parse_list POSIX format tests --

pub fn parse_list_posix_regular_file_test() {
  let line = "-rw-r--r-- 1 1000 1000 4096 Nov  5 2020 example.txt"
  let result = list.parse_list(line)
  let assert Ok(f) = result
  let assert "example.txt" = file.name(f)
  let assert file_type.File = file.file_type(f)
  let assert 4096 = file.size(f)
  let assert Some(1000) = file.uid(f)
  let assert Some(1000) = file.gid(f)
  let assert Some(FilePermissions(
    owner: PosixPermission(read: True, write: True, execute: False),
    group: PosixPermission(read: True, write: False, execute: False),
    others: PosixPermission(read: True, write: False, execute: False),
  )) = file.permissions(f)
  let assert Some(_) = file.modified(f)
  let assert None = file.symlink_target(f)
}

pub fn parse_list_posix_directory_test() {
  let line = "drwxr-xr-x 2 1000 1000 4096 Jan 10 2021 documents"
  let assert Ok(f) = list.parse_list(line)
  let assert "documents" = file.name(f)
  let assert file_type.Directory = file.file_type(f)
  let assert 4096 = file.size(f)
  let assert Some(_) = file.modified(f)
}

pub fn parse_list_posix_symlink_with_target_test() {
  let line = "lrwxrwxrwx 1 1000 1000 11 Jan 10 2021 link -> target.txt"
  let assert Ok(f) = list.parse_list(line)
  let assert "link" = file.name(f)
  let assert True = file.file_type(f) |> file_type.is_symlink
  let assert Some("target.txt") = file.symlink_target(f)
}

pub fn parse_list_posix_symlink_without_target_test() {
  // When a symlink line has no " -> target" part, it falls back to File type
  let line = "lrwxrwxrwx 1 1000 1000 11 Jan 10 2021 orphan_link"
  let assert Ok(f) = list.parse_list(line)
  let assert "orphan_link" = file.name(f)
  let assert file_type.File = file.file_type(f)
}

pub fn parse_list_posix_time_format_test() {
  // Time format with HH:mm instead of year
  let line = "-rw-r--r-- 1 1000 1000 1234 Nov  5 13:46 recent.txt"
  let assert Ok(f) = list.parse_list(line)
  let assert "recent.txt" = file.name(f)
  let assert 1234 = file.size(f)
  let assert Some(_) = file.modified(f)
}

pub fn parse_list_posix_all_permissions_test() {
  let line = "-rwxrwxrwx 1 1000 1000 0 Jan  1 2020 all_perms.sh"
  let assert Ok(f) = list.parse_list(line)
  let assert Some(FilePermissions(
    owner: PosixPermission(read: True, write: True, execute: True),
    group: PosixPermission(read: True, write: True, execute: True),
    others: PosixPermission(read: True, write: True, execute: True),
  )) = file.permissions(f)
}

pub fn parse_list_posix_no_permissions_test() {
  let line = "---------- 1 1000 1000 0 Jan  1 2020 no_perms.txt"
  let assert Ok(f) = list.parse_list(line)
  let assert Some(FilePermissions(
    owner: PosixPermission(read: False, write: False, execute: False),
    group: PosixPermission(read: False, write: False, execute: False),
    others: PosixPermission(read: False, write: False, execute: False),
  )) = file.permissions(f)
}

pub fn parse_list_posix_setuid_permissions_test() {
  let line = "-rwsr-xr-x 1 0 0 12345 Mar 15 2022 suid_program"
  let assert Ok(f) = list.parse_list(line)
  let assert Some(perms) = file.permissions(f)
  // 's' in execute position should count as execute
  let assert PosixPermission(read: True, write: True, execute: True) =
    perms.owner
}

pub fn parse_list_posix_sticky_bit_test() {
  let line = "drwxrwxrwt 2 0 0 4096 Jan  1 2020 tmp"
  let assert Ok(f) = list.parse_list(line)
  let assert Some(perms) = file.permissions(f)
  // 't' in execute position should count as execute
  let assert PosixPermission(read: True, write: True, execute: True) =
    perms.others
}

pub fn parse_list_posix_text_user_group_test() {
  // Many FTP servers return text user/group names instead of numeric UIDs
  let line = "-rw-r--r-- 1 user group 1234 Nov  5 2020 readme.txt"
  let assert Ok(f) = list.parse_list(line)
  let assert "readme.txt" = file.name(f)
  let assert None = file.uid(f)
  let assert None = file.gid(f)
}

pub fn parse_list_posix_numeric_uid_gid_test() {
  let line = "-rw-r--r-- 1 0 0 1234 Nov  5 2020 root_file.txt"
  let assert Ok(f) = list.parse_list(line)
  let assert Some(0) = file.uid(f)
  let assert Some(0) = file.gid(f)
}

pub fn parse_list_posix_filename_with_spaces_test() {
  let line = "-rw-r--r-- 1 1000 1000 1234 Nov  5 2020 my file with spaces.txt"
  let assert Ok(f) = list.parse_list(line)
  let assert "my file with spaces.txt" = file.name(f)
}

pub fn parse_list_posix_with_adjusted_year_for_future_dates_test() {
  let now = timestamp.system_time()
  let #(date, _) = timestamp.to_calendar(now, duration.milliseconds(0))
  let this_year = date.year

  let month = calendar.month_to_int(date.month)
  let new_month = month + 6
  let assert Ok(adjusted_month) = calendar.month_from_int(new_month)
  let new_date = calendar.Date(..date, month: adjusted_month)
  let assert Ok(new_date) = tempo_date.from_calendar_date(new_date)

  let date_fmt =
    tempo_date.format(new_date, tempo.CustomDate("MMM D")) <> " 14:30"

  let line_to_parse =
    "-rw-r--r-- 1 1000 1000 1234 " <> date_fmt <> " future_file.txt"

  let assert Ok(file) = list.parse_list(line_to_parse)

  let assert Some(parsed_date) = file.modified(file)
  let #(parsed_date, _) =
    timestamp.to_calendar(parsed_date, duration.milliseconds(0))
  assert this_year == parsed_date.year + 1
  assert adjusted_month == parsed_date.month
}

pub fn parse_list_posix_should_not_adjust_year_for_near_future_dates_test() {
  let now = timestamp.system_time()
  let #(date, _) = timestamp.to_calendar(now, duration.milliseconds(0))
  let this_year = date.year

  let month = calendar.month_to_int(date.month)
  let new_month = month + 2
  let assert Ok(adjusted_month) = calendar.month_from_int(new_month)
  let new_date = calendar.Date(..date, month: adjusted_month)
  let assert Ok(new_date) = tempo_date.from_calendar_date(new_date)

  let date_fmt =
    tempo_date.format(new_date, tempo.CustomDate("MMM D")) <> " 14:30"

  let line_to_parse =
    "-rw-r--r-- 1 1000 1000 1234 " <> date_fmt <> " future_file.txt"

  let assert Ok(file) = list.parse_list(line_to_parse)

  let assert Some(parsed_date) = file.modified(file)
  let #(parsed_date, _) =
    timestamp.to_calendar(parsed_date, duration.milliseconds(0))
  assert this_year == parsed_date.year
  assert adjusted_month == parsed_date.month
}

// -- parse_list DOS format tests --

pub fn parse_list_dos_directory_test() {
  let line = "10-19-20  03:19PM <DIR>          pub"
  let assert Ok(f) = list.parse_list(line)
  let assert "pub" = file.name(f)
  let assert file_type.Directory = file.file_type(f)
  let assert Some(_) = file.modified(f)
}

pub fn parse_list_dos_file_test() {
  let line = "04-08-14  03:09PM       403      readme.txt"
  let assert Ok(f) = list.parse_list(line)
  let assert "readme.txt" = file.name(f)
  let assert file_type.File = file.file_type(f)
  let assert 403 = file.size(f)
  let assert Some(_) = file.modified(f)
}

pub fn parse_list_dos_am_time_test() {
  let line = "01-15-21  08:30AM <DIR>          morning_dir"
  let assert Ok(f) = list.parse_list(line)
  let assert "morning_dir" = file.name(f)
  let assert file_type.Directory = file.file_type(f)
}

pub fn parse_list_dos_pm_time_with_space_test() {
  let line = "01-15-21  08:30 PM <DIR>          morning_dir"
  let assert Ok(f) = list.parse_list(line)
  let assert "morning_dir" = file.name(f)
  let assert file_type.Directory = file.file_type(f)
}

pub fn parse_list_dos_file_with_big_size_test() {
  let line = "04-08-14  03:09PM       1,234,403      readme.txt"
  let assert Ok(f) = list.parse_list(line)
  let assert "readme.txt" = file.name(f)
  let assert file_type.File = file.file_type(f)
  let assert 1_234_403 = file.size(f)
  let assert Some(_) = file.modified(f)
}

// -- parse_list error tests --

pub fn parse_list_empty_string_test() {
  let assert Error(list.SyntaxError) = list.parse_list("")
}

pub fn parse_list_garbage_test() {
  let assert Error(list.SyntaxError) =
    list.parse_list("this is not a valid list line")
}

// -- parse_mlsd / parse_mlst tests --

pub fn parse_mlsd_file_test() {
  let line = "type=file;size=1234;modify=20201105134600; example.txt"
  let assert Ok(f) = list.parse_mlsd(line)
  let assert "example.txt" = file.name(f)
  let assert file_type.File = file.file_type(f)
  let assert 1234 = file.size(f)
  let assert Some(_) = file.modified(f)
}

pub fn parse_mlsd_directory_test() {
  let line = "type=dir;size=4096;modify=20210110120000; documents"
  let assert Ok(f) = list.parse_mlsd(line)
  let assert "documents" = file.name(f)
  let assert file_type.Directory = file.file_type(f)
  let assert 4096 = file.size(f)
}

pub fn parse_mlsd_cdir_test() {
  let line = "type=cdir;modify=20210110120000; ."
  let assert Ok(f) = list.parse_mlsd(line)
  let assert "." = file.name(f)
  let assert file_type.Directory = file.file_type(f)
}

pub fn parse_mlsd_pdir_test() {
  let line = "type=pdir;modify=20210110120000; .."
  let assert Ok(f) = list.parse_mlsd(line)
  let assert ".." = file.name(f)
  let assert file_type.Directory = file.file_type(f)
}

pub fn parse_mlsd_link_test() {
  let line = "type=link;size=11;modify=20210110120000; symlink.txt"
  let assert Ok(f) = list.parse_mlsd(line)
  let assert "symlink.txt" = file.name(f)
  // Links are parsed as File type (we can't retrieve the target from MLSD)
  let assert file_type.File = file.file_type(f)
}

pub fn parse_mlsd_with_unix_uid_gid_test() {
  let line =
    "type=file;size=100;modify=20201105134600;unix.uid=1000;unix.gid=2000; data.bin"
  let assert Ok(f) = list.parse_mlsd(line)
  let assert Some(1000) = file.uid(f)
  let assert Some(2000) = file.gid(f)
}

pub fn parse_mlsd_with_unix_mode_test() {
  let line = "type=file;size=100;modify=20201105134600;unix.mode=755; script.sh"
  let assert Ok(f) = list.parse_mlsd(line)
  let assert Some(FilePermissions(
    owner: PosixPermission(read: True, write: True, execute: True),
    group: PosixPermission(read: True, write: False, execute: True),
    others: PosixPermission(read: True, write: False, execute: True),
  )) = file.permissions(f)
}

pub fn parse_mlsd_with_unix_mode_644_test() {
  let line =
    "type=file;size=100;modify=20201105134600;unix.mode=644; readme.txt"
  let assert Ok(f) = list.parse_mlsd(line)
  let assert Some(FilePermissions(
    owner: PosixPermission(read: True, write: True, execute: False),
    group: PosixPermission(read: True, write: False, execute: False),
    others: PosixPermission(read: True, write: False, execute: False),
  )) = file.permissions(f)
}

pub fn parse_mlsd_with_unix_mode_4digits_test() {
  let line =
    "type=file;size=100;modify=20201105134600;unix.mode=0644; readme.txt"
  let assert Ok(f) = list.parse_mlsd(line)
  let assert Some(FilePermissions(
    owner: PosixPermission(read: True, write: True, execute: False),
    group: PosixPermission(read: True, write: False, execute: False),
    others: PosixPermission(read: True, write: False, execute: False),
  )) = file.permissions(f)
}

pub fn parse_mlsd_unknown_keys_ignored_test() {
  let line =
    "type=file;size=100;modify=20201105134600;unknown.key=value; test.txt"
  let assert Ok(f) = list.parse_mlsd(line)
  let assert "test.txt" = file.name(f)
  let assert 100 = file.size(f)
}

pub fn parse_mlsd_case_insensitive_type_test() {
  let line = "Type=File;Size=100;Modify=20201105134600; test.txt"
  let assert Ok(f) = list.parse_mlsd(line)
  let assert file_type.File = file.file_type(f)
  let assert 100 = file.size(f)
}

pub fn parse_mlsd_empty_string_test() {
  let assert Error(list.SyntaxError) = list.parse_mlsd("")
}

pub fn parse_mlsd_whitespace_only_test() {
  let assert Error(list.SyntaxError) = list.parse_mlsd("   ")
}

pub fn parse_mlsd_invalid_type_test() {
  let line = "type=invalid;size=100; test.txt"
  let assert Error(list.SyntaxError) = list.parse_mlsd(line)
}

pub fn parse_mlsd_invalid_size_test() {
  let line = "type=file;size=notanumber; test.txt"
  let assert Error(list.BadSize) = list.parse_mlsd(line)
}

pub fn parse_mlsd_malformed_token_without_equals_test() {
  let line = "type=file;badtoken;size=100; test.txt"
  let assert Error(list.SyntaxError) = list.parse_mlsd(line)
}

pub fn parse_mlst_file_test() {
  let line = "type=file;size=2048;modify=20220315100000; report.pdf"
  let assert Ok(f) = list.parse_mlst(line)
  let assert "report.pdf" = file.name(f)
  let assert file_type.File = file.file_type(f)
  let assert 2048 = file.size(f)
}

pub fn parse_mlst_directory_test() {
  let line = "type=dir;modify=20220101000000; my_folder"
  let assert Ok(f) = list.parse_mlst(line)
  let assert "my_folder" = file.name(f)
  let assert file_type.Directory = file.file_type(f)
}
