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

pub fn main() -> Nil {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/gftp>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
