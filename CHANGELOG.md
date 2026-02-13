# Changelog

## 2.0.0

Released on 2026-02-13

- Added:
  - add OTP actor wrapper with chunk protection for safe streaming (#13)
  - add with_data_timeout setter and improve internal function docs
  - preserve original code in Unknown status and add to_int
  - make actor call timeout configurable
- Changed:
  - Moved private types to internal/ module (#12)
  - move IpVersion to public module and re-export Features
- Documentation:
  - Examples (#27)
  - update module doc install command to gftp@2
  - update CHANGELOG for v2.0.0 release
- Fixed:
  - use EPRT command for IPv6 connections in active mode (#20)
  - Handle possible space before AM/PM in DOS time format (#21)
  - parse_lstime adjusts year for future dates (#23)
  - ABOR must read `ClosingDataConnection` if first response is `TransferAborted` (#25)
  - MLST and MLSD `unix.mode` should accept 4-digit octal modes (#26)
  - add missing _test suffix to test functions and fix extract_str bug
  - send PBSZ and PROT after implicit FTPS connection
- Miscellaneous:
  - git-cliff (#22)
  - remove leftover hello_world_test scaffold
- Testing:
  - added a test to ensure DOS file size with commas are parsed correctly (#24)
