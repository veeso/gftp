# Changelog

## 2.0.0

Released on

- **Breaking changes**:
  - Moved internal modules under `gftp/internal/`: `command`, `command/feat`, `command/protection_level`, `listener`, `listener_ffi`, `utils`. These are no longer part of the public API.
  - Moved `gftp/command/file_type` to `gftp/file_type` (public, but changed import path).
  - Moved `open_retr`, `open_stor`, `open_appe`, `open_list`, `open_nlst`, `open_mlsd`, `open_data_command`, and `close_data_channel` from `gftp` to `gftp/internal/data_channel`. Use `gftp/actor` for message-based streaming instead.
- **New features**:
  - Added `gftp/actor` module: an OTP actor wrapper that serializes all FTP operations and prevents protocol state corruption by rejecting control commands while a data channel is open (`DataTransferInProgress` error).
  - Added `DataTransferInProgress` variant to `FtpError`.
- **New dependencies**:
  - `gleam_otp` >= 1.2.0
