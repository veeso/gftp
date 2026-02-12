# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

gftp is a Gleam FTP/FTPS client library implementing RFC 959, 2228, 4217, 2428, and 2389. It targets the Erlang VM and uses `mug` for TCP sockets and `kafein` for TLS/SSL. Based on the Rust library [suppaftp](https://github.com/veeso/suppaftp).

## Commands

- **Build:** `gleam build`
- **Run all tests:** `gleam test`
- **Run a single test:** `gleam test -- --only <test_function_name>` (gleeunit filter)
- **Format code:** `gleam format`
- **Check formatting:** `gleam format --check src test`
- **Download deps:** `gleam deps download`

CI runs: `gleam test` then `gleam format --check src test`.

## Architecture

```
src/
  gftp.gleam                — Library entry point (main module)
  gftp/
    actor.gleam              — OTP actor wrapper for safe, serialized FTP operations (public)
    file_type.gleam          — FileType and FormatControl types for the TYPE command (public)
    mode.gleam               — Mode type (Active/Passive)
    response.gleam           — FTP Response type and parsing
    result.gleam             — FtpResult type alias
    status.gleam             — FTP status codes
    stream.gleam             — DataStream type (TCP/TLS abstraction), StreamMessage type and message-based I/O
    stream_ffi.erl           — Erlang FFI for stream operations
    list.gleam               — FTP directory listing parsers (LIST, MLSD, MLST)
    list/
      file.gleam             — File type for parsed directory entries
      file_type.gleam        — FileType for directory entries
      permission.gleam       — Unix permission types
    internal/
      command.gleam          — FTP Command type and to_string encoding
      command/
        feat.gleam           — FEAT response parsing
        protection_level.gleam — ProtectionLevel type for PROT command
      data_channel.gleam     — Open/close data channel helpers (used by actor)
      listener.gleam         — TCP listener for active mode connections
      listener_ffi.erl       — Erlang FFI for listener
      utils.gleam            — Internal utility functions
```

- **Public vs Internal:** Modules under `gftp/internal/` are implementation details not intended for direct import by library users. Public modules (`file_type`, `mode`, `response`, `result`, `status`, `stream`, `list`) form the stable API.
- **Command encoding pattern:** Each command variant maps to its RFC wire format string via `to_string`. Sub-types (`FileType`, `ProtectionLevel`) have their own `to_string` functions in dedicated modules.
- **Callback vs message-based API:** Data transfer functions (`retr`, `stor`, `list`, etc.) use callbacks. For message-based I/O, use `gftp/actor` which exposes `open_*` / `close_data_channel` with chunk protection. The underlying data channel helpers live in `gftp/internal/data_channel`.
- **Actor wrapper:** `gftp/actor` wraps `FtpClient` in an OTP actor (gen_server) that serializes all operations and rejects control commands while a data channel is open (`DataTransferInProgress`). Uses `gleam_otp`.
- **Dependencies:** `mug` (TCP), `kafein` (TLS), `gleam_erlang` (process selectors), `gleam_otp` (actor), `gleam_stdlib`. Tests use `gleeunit`.
- **Target:** Erlang only (`target = "erlang"` in gleam.toml).

## Conventions

- Test files mirror source structure under `test/` with a `_test` suffix (e.g., `test/gftp/internal/command_test.gleam`).
- Integration tests must be run by setting `GFTP_INTEGRATION_TESTS=1` in the environment.
- Test functions must end with `_test` (gleeunit convention).
- Use `gleam/option.{type Option}` for optional command parameters (e.g., `List(Option(String))`).
- Always run `gleam format` before committing to ensure consistent code style.
- Always commits using conventional commit messages (e.g., `feat: add new FTP command`, `fix: correct command encoding`, `test: add tests for command encoding`).
