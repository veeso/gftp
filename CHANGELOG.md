# Changelog

## 2.0.0

Released on

- **Breaking changes**:
  - Moved internal modules under `gftp/internal/`: `command`, `command/feat`, `command/protection_level`, `listener`, `listener_ffi`, `utils`. These are no longer part of the public API.
  - Moved `gftp/command/file_type` to `gftp/file_type` (public, but changed import path).
