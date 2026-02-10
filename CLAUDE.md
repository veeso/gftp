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
  gftp.gleam              — Library entry point (main module)
  gftp/
    command.gleam          — FTP Command type (enum of all FTP commands) and encode_command/1
    command/
      file_type.gleam      — FileType and FormatControl types for the TYPE command
      protection_level.gleam — ProtectionLevel type for the PROT command
```

- **Command encoding pattern:** Each command variant maps to its RFC wire format string via `encode_command`. Sub-types (`FileType`, `ProtectionLevel`) have their own `to_string` functions in dedicated modules under `command/`.
- **Dependencies:** `mug` (TCP), `kafein` (TLS), `gleam_stdlib`. Tests use `gleeunit`.
- **Target:** Erlang only (`target = "erlang"` in gleam.toml).

## Conventions

- Test files mirror source structure under `test/` with a `_test` suffix (e.g., `test/gftp/command_test.gleam`).
- Integration tests must be run by setting `GFTP_INTEGRATION_TESTS=1` in the environment.
- Test functions must end with `_test` (gleeunit convention).
- Use `gleam/option.{type Option}` for optional command parameters (e.g., `List(Option(String))`).
- Always run `gleam format` before committing to ensure consistent code style.
- Always commits using conventional commit messages (e.g., `feat: add new FTP command`, `fix: correct command encoding`, `test: add tests for command encoding`).
