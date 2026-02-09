//// The set of FTP commands

import gftp/command/file_type.{type FileType}
import gftp/command/protection_level.{type ProtectionLevel}
import gleam/int
import gleam/option.{type Option}
import gleam/string

/// The `Command` type represents the different FTP commands that can be issued by the client.
/// Each command corresponds to a specific action that the FTP server can perform,
/// such as listing files, changing directories, or transferring files.
/// The `Command` type is designed to encapsulate all the necessary information for executing a command,
/// including any parameters or arguments that may be required.
pub type Command {
  /// Abort an active file transfer
  Abor
  /// Append to file
  Appe(String)
  /// Set auth to TLS
  Auth
  /// Ask server not to encrypt command channel
  ClearCommandChannel
  /// Change directory to parent directory
  Cdup
  /// Change working directory
  Cwd(String)
  /// Remove file at specified path
  Dele(String)
  /// Allows specification for protocol and address for data connections
  Eprt(host: String, port: Int, ip_version: IpVersion)
  /// Extended passive mode <https://www.rfc-editor.org/rfc/rfc2428#section-3>
  Epsv
  /// RFC 2389 <https://www.rfc-editor.org/rfc/rfc2389> list supported options on the server
  Feat
  /// List entries at specified path. If path is not provided list entries at current working directory
  List(Option(String))
  /// Get modification time for file at specified path
  Mdtm(String)
  /// Get the list of directories at specified path. If path is not provided list directories at current working directory
  Mlsd(Option(String))
  /// Get details of an individual file or directory at specified path
  Mlst(Option(String))
  /// Make directory
  Mkd(String)
  /// Get the list of file names at specified path. If path is not provided list entries at current working directory
  Nlst(Option(String))
  /// Ping server
  Noop
  /// RFC 2389 <https://www.rfc-editor.org/rfc/rfc2389>, Set option to server, syntax is (command-name, command-options)
  Opts(String, Option(String))
  /// Provide login password
  Pass(String)
  /// Passive mode
  Pasv
  /// Protection buffer size
  Pbsz(Int)
  /// Specifies an address and port to which the server should connect (active mode)
  Port(String)
  /// Set protection level for protocol
  Prot(ProtectionLevel)
  /// Print working directory
  Pwd
  /// Quit
  Quit
  /// Select file to rename
  RenameFrom(String)
  /// Rename selected file to
  RenameTo(String)
  /// Resume transfer from offset
  Rest(Int)
  /// Retrieve file
  Retr(String)
  /// Remove directory
  Rmd(String)
  /// Site command
  Site(String)
  /// Get file size of specified path
  Size(String)
  /// Put file at specified path
  Store(String)
  /// Set transfer file type
  Type(FileType)
  /// Provide user to login as
  User(String)
  /// Custom command
  Custom(String)
}

/// IP version; argument for `Eprt` command
pub type IpVersion {
  V4
  V6
}

/// Encode the `EPRT` command with the given host, port, and IP version.
fn encode_eprt(host: String, port: Int, ip_version: IpVersion) -> String {
  let ip_version_str = case ip_version {
    V4 -> "1"
    V6 -> "2"
  }

  ["EPRT", ip_version_str, host, int.to_string(port)]
  |> string.join("|")
}

pub fn encode_command(command: Command) -> String {
  case command {
    Abor -> "ABOR"
    Appe(f) -> "APPE " <> f
    Auth -> "AUTH TLS"
    Cdup -> "CDUP"
    ClearCommandChannel -> "CCC"
    Cwd(d) -> "CWD " <> d
    Dele(f) -> "DELE " <> f
    Eprt(host, port, ip_version) -> encode_eprt(host, port, ip_version)
    Epsv -> "EPSV"
    Feat -> "FEAT"
    List(p) ->
      p
      |> option.map(with: fn(x) { "LIST " <> x })
      |> option.unwrap(or: "LIST")
    Mdtm(p) -> "MDTM " <> p
    Mkd(p) -> "MKD " <> p
    Mlsd(p) ->
      p
      |> option.map(with: fn(x) { "MLSD " <> x })
      |> option.unwrap(or: "MLSD")
    Mlst(p) ->
      p
      |> option.map(with: fn(x) { "MLST " <> x })
      |> option.unwrap(or: "MLST")
    Nlst(p) ->
      p
      |> option.map(with: fn(x) { "NLST " <> x })
      |> option.unwrap(or: "NLST")
    Opts(command_name, command_opts) ->
      command_opts
      |> option.map(with: fn(opts) { "OPTS " <> command_name <> " " <> opts })
      |> option.unwrap(or: "OPTS " <> command_name)

    Noop -> "NOOP"
    Pass(p) -> "PASS " <> p
    Pasv -> "PASV"

    Pbsz(sz) -> "PBSZ " <> int.to_string(sz)
    Port(p) -> "PORT " <> p
    Prot(l) -> "PROT " <> protection_level.to_string(l)
    Pwd -> "PWD"
    Quit -> "QUIT"
    RenameFrom(p) -> "RNFR " <> p
    RenameTo(p) -> "RNTO " <> p
    Rest(offset) -> "REST " <> int.to_string(offset)
    Retr(p) -> "RETR " <> p
    Rmd(p) -> "RMD " <> p
    Site(p) -> "SITE " <> p
    Size(p) -> "SIZE " <> p
    Store(p) -> "STOR " <> p
    Type(t) -> "TYPE " <> file_type.to_string(t)
    User(u) -> "USER " <> u
    Custom(c) -> c
  }
}
