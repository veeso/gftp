import gftp/list
import gftp/list/file
import gftp/list/file_type
import gftp/list/permission.{FilePermissions, PosixPermission}
import gleam/option.{None, Some}
import gleeunit/should

// -- describe_error tests --

pub fn describe_error_syntax_error_test() {
  list.describe_error(list.SyntaxError)
  |> should.equal("the line does not conform to expected format.")
}

pub fn describe_error_invalid_date_test() {
  list.describe_error(list.InvalidDate)
  |> should.equal("the date field could not be parsed.")
}

pub fn describe_error_bad_size_test() {
  list.describe_error(list.BadSize)
  |> should.equal("the size field is not a valid unsigned integer.")
}

// -- parse_list POSIX format tests --

pub fn parse_list_posix_regular_file_test() {
  let line = "-rw-r--r-- 1 1000 1000 4096 Nov  5 2020 example.txt"
  let result = list.parse_list(line)
  let assert Ok(f) = result
  file.name(f) |> should.equal("example.txt")
  file.file_type(f) |> should.equal(file_type.File)
  file.size(f) |> should.equal(4096)
  file.uid(f) |> should.equal(Some(1000))
  file.gid(f) |> should.equal(Some(1000))
  file.permissions(f)
  |> should.equal(
    Some(FilePermissions(
      owner: PosixPermission(read: True, write: True, execute: False),
      group: PosixPermission(read: True, write: False, execute: False),
      others: PosixPermission(read: True, write: False, execute: False),
    )),
  )
  file.modified(f) |> should.be_some
  file.symlink_target(f) |> should.equal(None)
}

pub fn parse_list_posix_directory_test() {
  let line = "drwxr-xr-x 2 1000 1000 4096 Jan 10 2021 documents"
  let assert Ok(f) = list.parse_list(line)
  file.name(f) |> should.equal("documents")
  file.file_type(f) |> should.equal(file_type.Directory)
  file.size(f) |> should.equal(4096)
  file.modified(f) |> should.be_some
}

pub fn parse_list_posix_symlink_with_target_test() {
  let line = "lrwxrwxrwx 1 1000 1000 11 Jan 10 2021 link -> target.txt"
  let assert Ok(f) = list.parse_list(line)
  file.name(f) |> should.equal("link")
  file.file_type(f) |> file_type.is_symlink |> should.be_true
  file.symlink_target(f) |> should.equal(Some("target.txt"))
}

pub fn parse_list_posix_symlink_without_target_test() {
  // When a symlink line has no " -> target" part, it falls back to File type
  let line = "lrwxrwxrwx 1 1000 1000 11 Jan 10 2021 orphan_link"
  let assert Ok(f) = list.parse_list(line)
  file.name(f) |> should.equal("orphan_link")
  file.file_type(f) |> should.equal(file_type.File)
}

pub fn parse_list_posix_time_format_test() {
  // Time format with HH:mm instead of year
  let line = "-rw-r--r-- 1 1000 1000 1234 Nov  5 13:46 recent.txt"
  let assert Ok(f) = list.parse_list(line)
  file.name(f) |> should.equal("recent.txt")
  file.size(f) |> should.equal(1234)
  file.modified(f) |> should.be_some
}

pub fn parse_list_posix_all_permissions_test() {
  let line = "-rwxrwxrwx 1 1000 1000 0 Jan  1 2020 all_perms.sh"
  let assert Ok(f) = list.parse_list(line)
  file.permissions(f)
  |> should.equal(
    Some(FilePermissions(
      owner: PosixPermission(read: True, write: True, execute: True),
      group: PosixPermission(read: True, write: True, execute: True),
      others: PosixPermission(read: True, write: True, execute: True),
    )),
  )
}

pub fn parse_list_posix_no_permissions_test() {
  let line = "---------- 1 1000 1000 0 Jan  1 2020 no_perms.txt"
  let assert Ok(f) = list.parse_list(line)
  file.permissions(f)
  |> should.equal(
    Some(FilePermissions(
      owner: PosixPermission(read: False, write: False, execute: False),
      group: PosixPermission(read: False, write: False, execute: False),
      others: PosixPermission(read: False, write: False, execute: False),
    )),
  )
}

pub fn parse_list_posix_setuid_permissions_test() {
  let line = "-rwsr-xr-x 1 0 0 12345 Mar 15 2022 suid_program"
  let assert Ok(f) = list.parse_list(line)
  let assert Some(perms) = file.permissions(f)
  // 's' in execute position should count as execute
  perms.owner
  |> should.equal(PosixPermission(read: True, write: True, execute: True))
}

pub fn parse_list_posix_sticky_bit_test() {
  let line = "drwxrwxrwt 2 0 0 4096 Jan  1 2020 tmp"
  let assert Ok(f) = list.parse_list(line)
  let assert Some(perms) = file.permissions(f)
  // 't' in execute position should count as execute
  perms.others
  |> should.equal(PosixPermission(read: True, write: True, execute: True))
}

pub fn parse_list_posix_text_user_group_test() {
  // Many FTP servers return text user/group names instead of numeric UIDs
  let line = "-rw-r--r-- 1 user group 1234 Nov  5 2020 readme.txt"
  let assert Ok(f) = list.parse_list(line)
  file.name(f) |> should.equal("readme.txt")
  file.uid(f) |> should.equal(None)
  file.gid(f) |> should.equal(None)
}

pub fn parse_list_posix_numeric_uid_gid_test() {
  let line = "-rw-r--r-- 1 0 0 1234 Nov  5 2020 root_file.txt"
  let assert Ok(f) = list.parse_list(line)
  file.uid(f) |> should.equal(Some(0))
  file.gid(f) |> should.equal(Some(0))
}

pub fn parse_list_posix_filename_with_spaces_test() {
  let line = "-rw-r--r-- 1 1000 1000 1234 Nov  5 2020 my file with spaces.txt"
  let assert Ok(f) = list.parse_list(line)
  file.name(f) |> should.equal("my file with spaces.txt")
}

// -- parse_list DOS format tests --

pub fn parse_list_dos_directory_test() {
  let line = "10-19-20  03:19PM <DIR>          pub"
  let assert Ok(f) = list.parse_list(line)
  file.name(f) |> should.equal("pub")
  file.file_type(f) |> should.equal(file_type.Directory)
  file.modified(f) |> should.be_some
}

pub fn parse_list_dos_file_test() {
  let line = "04-08-14  03:09PM       403      readme.txt"
  let assert Ok(f) = list.parse_list(line)
  file.name(f) |> should.equal("readme.txt")
  file.file_type(f) |> should.equal(file_type.File)
  file.size(f) |> should.equal(403)
  file.modified(f) |> should.be_some
}

pub fn parse_list_dos_am_time_test() {
  let line = "01-15-21  08:30AM <DIR>          morning_dir"
  let assert Ok(f) = list.parse_list(line)
  file.name(f) |> should.equal("morning_dir")
  file.file_type(f) |> should.equal(file_type.Directory)
}

// -- parse_list error tests --

pub fn parse_list_empty_string_test() {
  list.parse_list("")
  |> should.equal(Error(list.SyntaxError))
}

pub fn parse_list_garbage_test() {
  list.parse_list("this is not a valid list line")
  |> should.equal(Error(list.SyntaxError))
}

// -- parse_mlsd / parse_mlst tests --

pub fn parse_mlsd_file_test() {
  let line = "type=file;size=1234;modify=20201105134600; example.txt"
  let assert Ok(f) = list.parse_mlsd(line)
  file.name(f) |> should.equal("example.txt")
  file.file_type(f) |> should.equal(file_type.File)
  file.size(f) |> should.equal(1234)
  file.modified(f) |> should.be_some
}

pub fn parse_mlsd_directory_test() {
  let line = "type=dir;size=4096;modify=20210110120000; documents"
  let assert Ok(f) = list.parse_mlsd(line)
  file.name(f) |> should.equal("documents")
  file.file_type(f) |> should.equal(file_type.Directory)
  file.size(f) |> should.equal(4096)
}

pub fn parse_mlsd_cdir_test() {
  let line = "type=cdir;modify=20210110120000; ."
  let assert Ok(f) = list.parse_mlsd(line)
  file.name(f) |> should.equal(".")
  file.file_type(f) |> should.equal(file_type.Directory)
}

pub fn parse_mlsd_pdir_test() {
  let line = "type=pdir;modify=20210110120000; .."
  let assert Ok(f) = list.parse_mlsd(line)
  file.name(f) |> should.equal("..")
  file.file_type(f) |> should.equal(file_type.Directory)
}

pub fn parse_mlsd_link_test() {
  let line = "type=link;size=11;modify=20210110120000; symlink.txt"
  let assert Ok(f) = list.parse_mlsd(line)
  file.name(f) |> should.equal("symlink.txt")
  // Links are parsed as File type (we can't retrieve the target from MLSD)
  file.file_type(f) |> should.equal(file_type.File)
}

pub fn parse_mlsd_with_unix_uid_gid_test() {
  let line =
    "type=file;size=100;modify=20201105134600;unix.uid=1000;unix.gid=2000; data.bin"
  let assert Ok(f) = list.parse_mlsd(line)
  file.uid(f) |> should.equal(Some(1000))
  file.gid(f) |> should.equal(Some(2000))
}

pub fn parse_mlsd_with_unix_mode_test() {
  let line = "type=file;size=100;modify=20201105134600;unix.mode=755; script.sh"
  let assert Ok(f) = list.parse_mlsd(line)
  file.permissions(f)
  |> should.equal(
    Some(FilePermissions(
      owner: PosixPermission(read: True, write: True, execute: True),
      group: PosixPermission(read: True, write: False, execute: True),
      others: PosixPermission(read: True, write: False, execute: True),
    )),
  )
}

pub fn parse_mlsd_with_unix_mode_644_test() {
  let line =
    "type=file;size=100;modify=20201105134600;unix.mode=644; readme.txt"
  let assert Ok(f) = list.parse_mlsd(line)
  file.permissions(f)
  |> should.equal(
    Some(FilePermissions(
      owner: PosixPermission(read: True, write: True, execute: False),
      group: PosixPermission(read: True, write: False, execute: False),
      others: PosixPermission(read: True, write: False, execute: False),
    )),
  )
}

pub fn parse_mlsd_unknown_keys_ignored_test() {
  let line =
    "type=file;size=100;modify=20201105134600;unknown.key=value; test.txt"
  let assert Ok(f) = list.parse_mlsd(line)
  file.name(f) |> should.equal("test.txt")
  file.size(f) |> should.equal(100)
}

pub fn parse_mlsd_case_insensitive_type_test() {
  let line = "Type=File;Size=100;Modify=20201105134600; test.txt"
  let assert Ok(f) = list.parse_mlsd(line)
  file.file_type(f) |> should.equal(file_type.File)
  file.size(f) |> should.equal(100)
}

pub fn parse_mlsd_empty_string_test() {
  list.parse_mlsd("")
  |> should.equal(Error(list.SyntaxError))
}

pub fn parse_mlsd_whitespace_only_test() {
  list.parse_mlsd("   ")
  |> should.equal(Error(list.SyntaxError))
}

pub fn parse_mlsd_invalid_type_test() {
  let line = "type=invalid;size=100; test.txt"
  list.parse_mlsd(line)
  |> should.equal(Error(list.SyntaxError))
}

pub fn parse_mlsd_invalid_size_test() {
  let line = "type=file;size=notanumber; test.txt"
  list.parse_mlsd(line)
  |> should.equal(Error(list.BadSize))
}

pub fn parse_mlsd_malformed_token_without_equals_test() {
  let line = "type=file;badtoken;size=100; test.txt"
  list.parse_mlsd(line)
  |> should.equal(Error(list.SyntaxError))
}

pub fn parse_mlst_file_test() {
  let line = "type=file;size=2048;modify=20220315100000; report.pdf"
  let assert Ok(f) = list.parse_mlst(line)
  file.name(f) |> should.equal("report.pdf")
  file.file_type(f) |> should.equal(file_type.File)
  file.size(f) |> should.equal(2048)
}

pub fn parse_mlst_directory_test() {
  let line = "type=dir;modify=20220101000000; my_folder"
  let assert Ok(f) = list.parse_mlst(line)
  file.name(f) |> should.equal("my_folder")
  file.file_type(f) |> should.equal(file_type.Directory)
}
