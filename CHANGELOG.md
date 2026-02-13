# Changelog

## 2.0.0

Released on 2026-02-13

- **Breaking changes**:
  - Moved internal modules under `gftp/internal/`: `command`, `command/feat`, `command/protection_level`, `listener`, `listener_ffi`, `utils`. These are no longer part of the public API.
  - Moved `gftp/command/file_type` to `gftp/file_type` (public, but changed import path).
  - Moved `open_retr`, `open_stor`, `open_appe`, `open_list`, `open_nlst`, `open_mlsd`, `open_data_command`, and `close_data_channel` from `gftp` to `gftp/internal/data_channel`. Use `gftp/actor` for message-based streaming instead.
  - `IpVersion` type moved from `gftp/internal/command` to `gftp/mode`.
  - `Features` type is now re-exported from `gftp` (was only available via `gftp/internal/command/feat`).
  - `Status.Unknown` now carries the original status code: `Unknown(Int)` instead of `Unknown`.
  - `gftp/actor.Handle` is now an opaque type (was a type alias for `Subject(Message)`).
- **New features**:
  - Added `gftp/actor` module: an OTP actor wrapper that serializes all FTP operations and prevents protocol state corruption by rejecting control commands while a data channel is open (`DataTransferInProgress` error).
  - Added `DataTransferInProgress` variant to `FtpError`.
  - Added `status.to_int` to convert a `Status` back to its integer code.
  - Added `gftp.with_data_timeout` to configure the data channel timeout.
  - Added `gftp/actor.with_call_timeout` to configure the actor call timeout.
  - `connect_secure_implicit` now automatically sends `PBSZ 0` and `PROT P` for data channel protection.
- **Bug fixes**:
  - Fixed `extract_str` returning `Ok("")` instead of `Error(Nil)` when the token is not present.
- **New dependencies**:
  - `gleam_otp` >= 1.2.0
