# GFTP - Gleam FTP Client Library

[![license-mit](https://img.shields.io/badge/license-MIT-teal.svg)](https://opensource.org/licenses/MIT)
[![repo-stars](https://img.shields.io/github/stars/veeso/gftp?style=flat)](https://github.com/veeso/gftp/stargazers)
[![Package Version](https://img.shields.io/hexpm/v/gftp)](https://hex.pm/packages/gftp)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gftp/)

[![ci](https://github.com/veeso/gftp/actions/workflows/test.yml/badge.svg)](https://github.com/veeso/gftp/actions)

## Overview

Gleam FTP (gftp) is a Gleam client library for FTP (File Transfer Protocol) and FTPS (FTP over SSL/TLS) with full RFC compliance. It runs on the Erlang VM and provides a simple, type-safe API for all common FTP operations.

Based on the Rust FTP library [suppaftp](https://github.com/veeso/suppaftp).

### Features

- FTP and FTPS (explicit and implicit) support
- Passive, Extended Passive (EPSV), and Active data transfer modes
- NAT workaround for passive mode behind firewalls
- Directory listing parsing (POSIX, DOS, MLSD/MLST formats)
- File upload, download, append, rename, and delete
- Directory creation, removal, and navigation
- File size and modification time queries
- Server feature discovery (FEAT/OPTS, RFC 2389)
- Custom command support for server-specific extensions
- OTP actor wrapper for safe concurrent use and chunk protection during data transfers
- Full RFC compliance: [959](https://tools.ietf.org/html/rfc959), [2228](https://tools.ietf.org/html/rfc2228), [4217](https://tools.ietf.org/html/rfc4217), [2428](https://tools.ietf.org/html/rfc2428), [2389](https://tools.ietf.org/html/rfc2389)

### Requirements

- Erlang/OTP (target = erlang)
- Gleam >= 1.14.0

## Installation

```sh
gleam add gftp@2
```

## Quick Start

### Direct use of `FtpClient`

You can both use `FtpClient` directly for simple command/response operations and callback-based data transfers, or the `gftp/actor` wrapper for safe concurrent use in OTP actors with message-based streaming. Here's a quick example using `FtpClient` directly:

```gleam
import gftp
import gftp/file_type
import gftp/stream
import gftp/result as ftp_result
import gleam/bit_array
import gleam/option.{None}
import gleam/result

pub fn main() {
  // Connect and login
  let assert Ok(client) = gftp.connect("ftp.example.com", 21)
  let assert Ok(_) = gftp.login(client, "user", "password")

  // Set binary transfer type
  let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)

  // Upload a file
  let assert Ok(_) = gftp.stor(client, "hello.txt", fn(data_stream) {
    stream.send(data_stream, bit_array.from_string("Hello, world!"))
    |> result.map_error(ftp_result.Socket)
  })

  // List current directory
  let assert Ok(_entries) = gftp.list(client, None)

  // Download a file
  let assert Ok(_) = gftp.retr(client, "hello.txt", fn(data_stream) {
    let assert Ok(_data) = stream.receive(data_stream, 5000)
    Ok(Nil)
  })

  // Quit and shutdown
  let assert Ok(_) = gftp.quit(client)
  let assert Ok(_) = gftp.shutdown(client)
}
```

### Usage with OTP Actor

The actor wrapper (`gftp/actor`) provides two benefits over using `FtpClient` directly:

1. **Chunk protection** — control commands are automatically rejected with `DataTransferInProgress` while a data channel is open, preventing FTP protocol state corruption.
2. **Message-based streaming** — open a data channel with `open_retr`, `open_stor`, etc. and receive data as messages in your OTP actor's inbox via `stream.select_stream_messages`. This lets you interleave FTP data packets with other message types (timers, other sockets, application events) using a `process.Selector`.

If you only need simple callback-based transfers (like `gftp.retr`, `gftp.stor`), using `FtpClient` directly is perfectly fine.

#### Basic usage

```gleam
import gftp
import gftp/actor as ftp_actor
import gftp/stream
import gftp/result as ftp_result
import gleam/bit_array
import gleam/result

// Connect and wrap in an actor
let assert Ok(client) = gftp.connect("ftp.example.com", 21)
let assert Ok(started) = ftp_actor.start(client)
let handle = started.data

// All operations go through the actor handle
let assert Ok(_) = ftp_actor.login(handle, "user", "password")
let assert Ok(cwd) = ftp_actor.pwd(handle)

// Callback-based transfers work the same as with FtpClient
let assert Ok(_) = ftp_actor.stor(handle, "hello.txt", fn(data_stream) {
  stream.send(data_stream, bit_array.from_string("Hello, world!"))
  |> result.map_error(ftp_result.Socket)
})

let assert Ok(_) = ftp_actor.quit(handle)
```

#### Message-based streaming with selectors

Use `open_*` to get a `DataStream`, then receive packets as messages in your own actor:

```gleam
import gftp/actor as ftp_actor
import gftp/stream.{type StreamMessage, Packet, StreamClosed, StreamError}
import gleam/erlang/process

// Open a data channel — control commands are blocked until close
let assert Ok(data_stream) = ftp_actor.open_retr(handle, "large_file.bin")

// Request the first packet to be delivered as a message
stream.receive_next_packet_as_message(data_stream)

// Build a selector that handles FTP stream messages alongside your own messages
let selector =
  process.new_selector()
  |> stream.select_stream_messages(fn(msg) { msg })

// Receive packets in a loop
case process.select(selector, 30_000) {
  Ok(Packet(data)) -> {
    // Process chunk, then request the next one
    stream.receive_next_packet_as_message(data_stream)
    // ... continue selecting ...
  }
  Ok(StreamClosed) -> {
    // Transfer complete — close the data channel to unblock control commands
    let assert Ok(_) = ftp_actor.close_data_channel(handle, data_stream)
  }
  Ok(StreamError(err)) -> // handle error
  Error(_) -> // timeout
}
```

## Usage Guide

### Connecting

```gleam
import gftp

// Connect with default 30s timeout
let assert Ok(client) = gftp.connect("ftp.example.com", 21)

// Connect with custom timeout (in milliseconds)
let assert Ok(client) = gftp.connect_timeout("ftp.example.com", 21, timeout: 10_000)
```

### FTPS (Secure FTP)

#### Explicit FTPS (recommended)

Connect over plain FTP, then upgrade the connection to TLS:

```gleam
import gftp
import kafein

let assert Ok(client) = gftp.connect("ftp.example.com", 21)
let ssl_options = kafein.WrapOptions(
  server_name_indication: kafein.SniEnabled("ftp.example.com"),
  // ... other TLS options
)
let assert Ok(client) = gftp.into_secure(client, ssl_options)
let assert Ok(_) = gftp.login(client, "user", "password")
```

#### Implicit FTPS (legacy)

Connect directly over TLS (typically on port 990):

```gleam
import gftp
import kafein

let ssl_options = kafein.WrapOptions(
  server_name_indication: kafein.SniEnabled("ftp.example.com"),
  // ... other TLS options
)
let assert Ok(client) = gftp.connect_secure_implicit("ftp.example.com", 990, ssl_options, 30_000)
```

### Data Transfer Modes

gftp defaults to **passive mode**, which works in most environments. You can switch modes as needed:

```gleam
import gftp
import gftp/mode

// Passive mode (default) - client connects to server for data transfer
let client = gftp.with_mode(client, mode.Passive)

// Extended passive mode (RFC 2428) - required by some servers, supports IPv6
let client = gftp.with_mode(client, mode.ExtendedPassive)

// Active mode - server connects back to client (30s timeout for the connection)
let client = gftp.with_active_mode(client, 30_000)

// Enable NAT workaround for passive mode behind firewalls
let client = gftp.with_nat_workaround(client, True)
```

### Directory Operations

```gleam
import gftp
import gleam/option.{None, Some}

// Print working directory
let assert Ok(cwd) = gftp.pwd(client)

// Change directory
let assert Ok(_) = gftp.cwd(client, "/pub/data")

// Go to parent directory
let assert Ok(_) = gftp.cdup(client)

// Create and remove directories
let assert Ok(_) = gftp.mkd(client, "new_folder")
let assert Ok(_) = gftp.rmd(client, "old_folder")
```

### File Operations

```gleam
import gftp
import gftp/stream
import gftp/result as ftp_result
import gleam/bit_array
import gleam/result

// Upload a file
let assert Ok(_) = gftp.stor(client, "upload.txt", fn(data_stream) {
  stream.send(data_stream, bit_array.from_string("file contents"))
  |> result.map_error(ftp_result.Socket)
})

// Download a file
let assert Ok(_) = gftp.retr(client, "download.txt", fn(data_stream) {
  let assert Ok(data) = stream.receive(data_stream, 5000)
  // process data...
  Ok(Nil)
})

// Append to a file
let assert Ok(_) = gftp.appe(client, "log.txt", fn(data_stream) {
  stream.send(data_stream, bit_array.from_string("new log entry\n"))
  |> result.map_error(ftp_result.Socket)
})

// Delete a file
let assert Ok(_) = gftp.dele(client, "old_file.txt")

// Rename a file
let assert Ok(_) = gftp.rename(client, "old_name.txt", "new_name.txt")

// Get file size and modification time
let assert Ok(size) = gftp.size(client, "file.txt")
let assert Ok(mtime) = gftp.mdtm(client, "file.txt")
```

### Directory Listings

gftp provides multiple listing commands and parsers for structured output:

```gleam
import gftp
import gftp/list as gftp_list
import gftp/list/file
import gftp/list/file_type
import gleam/list
import gleam/option.{None, Some}

// LIST command (human-readable format)
let assert Ok(lines) = gftp.list(client, None)
let assert Ok(files) = list.try_map(lines, gftp_list.parse_list)

// MLSD command (machine-readable, RFC 3659)
let assert Ok(lines) = gftp.mlsd(client, None)
let assert Ok(files) = list.try_map(lines, gftp_list.parse_mlsd)

// MLST command (single file info, RFC 3659)
let assert Ok(line) = gftp.mlst(client, Some("file.txt"))
let assert Ok(f) = gftp_list.parse_mlst(line)

// NLST command (file names only)
let assert Ok(names) = gftp.nlst(client, None)

// Access file metadata
let name = file.name(f)
let size = file.size(f)
let modified = file.modified(f)
let is_dir = file_type.is_directory(file.file_type(f))
```

### Server Features

```gleam
import gftp
import gleam/dict
import gleam/option.{None, Some}

// Discover server capabilities (RFC 2389)
let assert Ok(features) = gftp.feat(client)

// Check if a specific feature is supported
case dict.get(features, "MLST") {
  Ok(Some(params)) -> // MLST supported with params
  Ok(None) -> // MLST supported without params
  Error(_) -> // MLST not supported
}

// Set server options
let assert Ok(_) = gftp.opts(client, "UTF8", Some("ON"))
```

### Error Handling

All operations return `FtpResult(a)`, which is `Result(a, FtpError)`:

```gleam
import gftp
import gftp/result

case gftp.cwd(client, "/nonexistent") {
  Ok(_) -> // success
  Error(err) -> {
    // Get a human-readable error description
    let description = result.describe_error(err)
    // Match on specific error types
    case err {
      result.UnexpectedResponse(response) -> // server rejected the command
      result.ConnectionError(_) -> // connection failed
      result.Tls(_) -> // TLS error
      result.Socket(_) -> // socket error
      result.BadResponse -> // malformed server response
      _ -> // other errors
    }
  }
}
```

### Custom Commands

For server-specific commands not covered by the API:

```gleam
import gftp
import gftp/status

// Send a custom command
let assert Ok(response) = gftp.custom_command(client, "SITE CHMOD 755 file.txt", [status.CommandOk])

// Send a custom command that uses a data connection
let assert Ok(_) = gftp.custom_data_command(
  client,
  "LIST -la",
  [status.AboutToSend, status.AlreadyOpen],
  fn(data_stream, _response) {
    let assert Ok(lines) = gftp.read_lines_from_stream(data_stream, 5000)
    // process lines...
    Ok(Nil)
  },
)
```

### Naming Convention

Function names follow FTP command names for familiarity with the protocol:
`cwd` (Change Working Directory), `pwd` (Print Working Directory), `mkd` (Make Directory),
`rmd` (Remove Directory), `dele` (Delete), `retr` (Retrieve), `stor` (Store),
`appe` (Append), `nlst` (Name List), `mdtm` (Modification Time), etc.

## API Documentation

Full API documentation is available at <https://hexdocs.pm/gftp>.

## Development

```sh
gleam build        # Build the project
gleam test         # Run unit tests
gleam format       # Format code
```

### Integration tests

Integration tests run against a real FTP server inside a Docker container.
You need **Docker** installed and running on your machine to execute them.

```sh
GFTP_INTEGRATION_TESTS=1 gleam test
```

To also run active mode tests:

```sh
GFTP_INTEGRATION_TESTS=1 GFTP_ACTIVE_MODE_TESTS=1 gleam test
```

## git-cliff

You can generate a changelog entry before a new release by running:

```sh
git cliff -u --tag vX.Y.Z
```

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
