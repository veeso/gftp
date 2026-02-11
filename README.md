# GFTP - Gleam FTP Client Library

[![license-mit](https://img.shields.io/badge/license-MIT-teal.svg)](https://opensource.org/licenses/MIT)
[![repo-stars](https://img.shields.io/github/stars/veeso/gftp?style=flat)](https://github.com/veeso/gftp/stargazers)
[![Package Version](https://img.shields.io/hexpm/v/gftp)](https://hex.pm/packages/gftp)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gftp/)

[![ci](https://github.com/veeso/gftp/actions/workflows/test.yml/badge.svg)](https://github.com/veeso/gftp/actions)

## Overview

Gleam FTP (gftp) is Gleam Client library for FTP (File Transfer Protocol) and FTPS (FTP over SSL/TLS) with full [RFC 959](https://tools.ietf.org/html/rfc959), [RFC 2228](https://tools.ietf.org/html/rfc2228), [RFC 4217](https://tools.ietf.org/html/rfc4217), [RFC 2428](https://tools.ietf.org/html/rfc2428) and [RFC 2389](https://tools.ietf.org/html/rfc2389) compliance.

It is based on my Rust FTP library [suppaftp](https://github.com/veeso/suppaftp).

## Add gftp to your project

```sh
gleam add gftp@1
```

```gleam
import gftp
import gftp/command/file_type
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
  let assert Ok(entries) = gftp.list(client, None)

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

Further documentation can be found at <https://hexdocs.pm/gftp>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

### Integration tests

Integration tests run against a real FTP server inside a Docker container.
You need **Docker** installed and running on your machine to execute them.

```sh
GFTP_INTEGRATION_TESTS=1 gleam test
```

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
