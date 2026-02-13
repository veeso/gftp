//// GFTP - A Gleam FTP/FTPS client library with full RFC support for both passive and active mode
////
//// Gleam FTP (gftp) is a Gleam client library for FTP (File Transfer Protocol) and FTPS (FTP over SSL/TLS) with full
//// RFC 959, RFC 2228, RFC 4217, RFC 2428 and RFC 2389 compliance. It runs on the Erlang VM.
////
//// ## Add gftp to your project
////
//// ```bash
//// gleam add gftp@2
//// ```
////
//// ## Quick start
////
//// ```gleam
//// import gftp
//// import gftp/file_type
//// import gftp/stream
//// import gftp/result as ftp_result
//// import gleam/bit_array
//// import gleam/option.{None}
//// import gleam/result
////
//// pub fn main() {
////   // Connect and login
////   let assert Ok(client) = gftp.connect("ftp.example.com", 21)
////   let assert Ok(_) = gftp.login(client, "user", "password")
////
////   // Set binary transfer type
////   let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
////
////   // Upload a file
////   let assert Ok(_) = gftp.stor(client, "hello.txt", fn(data_stream) {
////     stream.send(data_stream, bit_array.from_string("Hello, world!"))
////     |> result.map_error(ftp_result.Socket)
////   })
////
////   // List current directory
////   let assert Ok(_entries) = gftp.list(client, None)
////
////   // Download a file
////   let assert Ok(_) = gftp.retr(client, "hello.txt", fn(data_stream) {
////     let assert Ok(_data) = stream.receive(data_stream, 5000)
////     Ok(Nil)
////   })
////
////   // Quit and shutdown
////   let assert Ok(_) = gftp.quit(client)
////   let assert Ok(_) = gftp.shutdown(client)
//// }
//// ```
////
//// ## OTP actor (safe message-based streaming)
////
//// For message-based, non-blocking data transfers, use `gftp/actor` which wraps
//// the FTP client in an OTP actor. The actor serializes all operations and
//// rejects control commands while a data channel is open (`DataTransferInProgress`).
////
//// ```gleam
//// import gftp
//// import gftp/actor as ftp_actor
////
//// let assert Ok(client) = gftp.connect("ftp.example.com", 21)
//// let assert Ok(started) = ftp_actor.start(client)
//// let handle = started.data
////
//// let assert Ok(_) = ftp_actor.login(handle, "user", "password")
//// let assert Ok(data_stream) = ftp_actor.open_retr(handle, "file.txt")
//// // ... receive data via stream.receive_next_packet_as_message ...
//// let assert Ok(_) = ftp_actor.close_data_channel(handle, data_stream)
//// let assert Ok(_) = ftp_actor.quit(handle)
//// ```
////
//// ## FTPS (Secure FTP)
////
//// To use explicit FTPS, connect normally and then upgrade the connection:
////
//// ```gleam
//// let assert Ok(client) = gftp.connect("ftp.example.com", 21)
//// let assert Ok(client) = gftp.into_secure(client, ssl_options)
//// let assert Ok(_) = gftp.login(client, "user", "password")
//// ```
////
//// ## Data transfer modes
////
//// gftp defaults to passive mode. You can switch to active or extended passive mode:
////
//// ```gleam
//// import gftp/mode
////
//// // Extended passive mode (RFC 2428, supports IPv6)
//// let client = gftp.with_mode(client, mode.ExtendedPassive)
////
//// // Active mode with 30s timeout
//// let client = gftp.with_active_mode(client, 30_000)
////
//// // Enable NAT workaround for passive mode behind firewalls
//// let client = gftp.with_nat_workaround(client, True)
//// ```
////
//// ## Directory listings
////
//// Parse structured file metadata from LIST or MLSD output:
////
//// ```gleam
//// import gftp/list as gftp_list
//// import gftp/list/file
//// import gleam/list
////
//// let assert Ok(lines) = gftp.list(client, None)
//// let assert Ok(files) = list.try_map(lines, gftp_list.parse_list)
//// let name = file.name(files |> list.first |> result.unwrap(file.empty()))
//// ```
////
//// ## Error handling
////
//// All operations return `FtpResult(a)` which is `Result(a, FtpError)`.
//// Use `gftp/result.describe_error` for human-readable messages:
////
//// ```gleam
//// import gftp/result
////
//// case gftp.cwd(client, "/nonexistent") {
////   Ok(_) -> // success
////   Error(err) -> {
////     let msg = result.describe_error(err)
////   }
//// }
//// ```
////
//// ## Naming convention
////
//// Function names follow FTP command names (`cwd`, `pwd`, `mkd`, `rmd`, `dele`, `retr`, `stor`, etc.)
//// for direct mapping to the FTP protocol.
////

import gftp/file_type
import gftp/internal/command.{type Command}
import gftp/internal/command/feat
import gftp/internal/command/protection_level
import gftp/internal/listener
import gftp/internal/utils
import gftp/mode.{type IpVersion, type Mode}
import gftp/response.{type Response, Response}
import gftp/result.{type FtpResult} as ftp_result
import gftp/status.{type Status}
import gftp/stream.{type DataStream}
import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/result
import gleam/string
import gleam/time/calendar
import gleam/time/timestamp.{type Timestamp}
import kafein
import mug
import tempo/duration

/// Default timeout for FTP connection in milliseconds (30 seconds).
const default_timeout = 30_000

/// Regular expression to parse the modification time of a file from the response of the MDTM command.
const mdtm_regex = "\\b(\\d{4})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})\\b"

/// Regular expression to parse the response of the PASV command, which contains the IP address and port for the data connection in passive mode.
const pasv_port_regex = "\\((\\d+),(\\d+),(\\d+),(\\d+),(\\d+),(\\d+)\\)"

/// Regular expression to parse the response of the EPSV command, which contains the port for the data connection in extended passive mode.
const epsv_port_regex = "\\(\\|\\|\\|(\\d+)\\|\\)"

/// Regular expression to parse the size of the file from the response of the SIZE command.
const size_regex = "\\s+(\\d+)\\s*$"

/// The features supported by the server, as returned by the FEAT command (RFC 2389).
///
/// A feature has a key representing the name of the feature, and an optional value
/// representing any parameters for that feature.
pub type Features =
  feat.Features

/// A function that creates a new stream for the data connection in passive mode.
///
/// It takes the socket address and port as arguments and returns a `FtpResult` containing either a `DataStream` or an `FtpError`.
pub type PassiveStreamBuilder =
  fn(String, Int) -> FtpResult(DataStream)

/// A client to interact with an FTP server. This is the main entry point for using gftp.
pub opaque type FtpClient {
  FtpClient(
    /// The data stream for the control connection. This is used to send commands and receive responses from the server.
    data_stream: DataStream,
    /// The current mode of the client (active or passive).
    mode: Mode,
    /// A flag to enable the NAT workaround
    nat_workaround: Bool,
    /// The server welcome message, if any.
    welcome_message: Option(String),
    /// The function to create a new stream for the data connection in passive mode.
    /// This is used when the client is in passive mode and needs to establish a data connection with the server.
    passive_stream_builder: PassiveStreamBuilder,
    /// TLS options for secure connections.
    /// This is used to upgrade the data stream to a secure connection using SSL/TLS when TLS is enabled.
    tls_options: Option(kafein.WrapOptions),
    /// Data timeout in milliseconds for data connections (both active and passive)
    data_timeout: Int,
  )
}

/// Try to connect to the remote server with the default timeout of 30 seconds.
///
/// Connects to the FTP server at the specified `host` and `port`, reads the server's welcome message,
/// and returns an `FtpClient` instance. You must call `login` before performing any FTP operations.
///
/// ```gleam
/// let assert Ok(client) = gftp.connect("ftp.example.com", 21)
/// let assert Ok(_) = gftp.login(client, "user", "password")
/// ```
pub fn connect(host: String, port: Int) -> FtpResult(FtpClient) {
  // Default timeout of 30 seconds
  connect_timeout(host, port, timeout: default_timeout)
}

/// Try to connect to the remote server with a custom timeout.
///
/// The `timeout` parameter specifies the maximum time to wait for a connection to be established, in milliseconds.
/// You must call `login` before performing any FTP operations.
///
/// ```gleam
/// let assert Ok(client) = gftp.connect_timeout("ftp.example.com", 21, timeout: 10_000)
/// ```
pub fn connect_timeout(
  host: String,
  port port: Int,
  timeout timeout: Int,
) -> FtpResult(FtpClient) {
  mug.ConnectionOptions(
    host: host,
    port: port,
    timeout: timeout,
    ip_version_preference: mug.Ipv6Preferred,
  )
  |> mug.connect()
  |> result.map_error(ftp_result.ConnectionError)
  |> result.try(fn(socket) { connect_with_stream(socket) })
}

/// Connect to the FTP server using an existing socket stream.
/// 
/// This method doesn't authenticate with the server, so after a successful connection, you will need to call the `login`
/// method to authenticate before performing any FTP operations.
/// 
/// On success, returns a `FtpClient` instance that can be used to interact with the FTP server.
/// On failure, returns an `FtpError` describing the issue.
pub fn connect_with_stream(stream: mug.Socket) -> FtpResult(FtpClient) {
  let client =
    FtpClient(
      data_stream: stream.Tcp(stream),
      mode: mode.Passive,
      nat_workaround: False,
      welcome_message: None,
      passive_stream_builder: default_passive_stream_builder,
      tls_options: None,
      data_timeout: default_timeout,
    )

  read_welcome_message(client)
}

/// Get the welcome message from the FTP server, if available.
pub fn welcome_message(ftp_client: FtpClient) -> Option(String) {
  ftp_client.welcome_message
}

/// Enable active mode for the FTP client with the specified timeout (in milliseconds) for the data connection.
///
/// In active mode, the client opens a local port and the server connects back to it for data transfer.
///
/// ```gleam
/// let client = gftp.with_active_mode(client, 30_000)
/// ```
pub fn with_active_mode(ftp_client: FtpClient, timeout: Int) -> FtpClient {
  FtpClient(..ftp_client, mode: mode.Active(timeout))
}

/// Set the passive stream builder function for the FTP client.
/// This function is used to create a new stream for the data connection in passive mode.
pub fn with_passive_stream_builder(
  ftp_client: FtpClient,
  builder: PassiveStreamBuilder,
) -> FtpClient {
  FtpClient(..ftp_client, passive_stream_builder: builder)
}

/// Set the timeout in milliseconds for data channel operations (reads/writes on the data stream).
/// Defaults to 30000 (30 seconds).
///
/// ```gleam
/// let client = gftp.with_data_timeout(client, 60_000)
/// ```
pub fn with_data_timeout(ftp_client: FtpClient, timeout: Int) -> FtpClient {
  FtpClient(..ftp_client, data_timeout: timeout)
}

/// Set the data transfer mode (Passive, ExtendedPassive, or Active).
///
/// ```gleam
/// import gftp/mode
///
/// let client = gftp.with_mode(client, mode.ExtendedPassive)
/// ```
pub fn with_mode(ftp_client: FtpClient, mode: Mode) -> FtpClient {
  FtpClient(..ftp_client, mode: mode)
}

/// Enable or disable the NAT workaround for passive mode.
///
/// When enabled, the client uses the control connection's peer address instead of the address
/// returned by the PASV command for data connections. This is useful when the server is behind
/// a NAT and reports its internal IP address.
///
/// ```gleam
/// let client = gftp.with_nat_workaround(client, True)
/// ```
pub fn with_nat_workaround(
  ftp_client: FtpClient,
  nat_workaround: Bool,
) -> FtpClient {
  FtpClient(..ftp_client, nat_workaround: nat_workaround)
}

/// Switch to explicit secure mode (FTPS) using the provided SSL configuration.
///
/// Sends AUTH TLS, upgrades the connection to SSL/TLS, sets PBSZ to 0 and PROT to Private.
/// This method does nothing if the connection is already secured.
///
/// ```gleam
/// let assert Ok(client) = gftp.connect("ftp.example.com", 21)
/// let assert Ok(client) = gftp.into_secure(client, ssl_options)
/// let assert Ok(_) = gftp.login(client, "user", "password")
/// ```
pub fn into_secure(
  ftp_client: FtpClient,
  ssl_options: kafein.WrapOptions,
) -> FtpResult(FtpClient) {
  // send auth and get Auth Ok response
  use _ <- result.try(perform(ftp_client, command.Auth))
  use _ <- result.try(read_response(ftp_client, status.AuthOk))
  // upgrade to ssl and create new client with the upgraded stream
  use stream <- result.try(
    ftp_client.data_stream
    |> stream.upgrade_to_ssl(ssl_options)
    |> result.map_error(ftp_result.Tls),
  )
  let ftp_client =
    FtpClient(..ftp_client, data_stream: stream, tls_options: Some(ssl_options))
  // Set protection buffer size
  use _ <- result.try(perform(ftp_client, command.Pbsz(0)))
  use _ <- result.try(read_response(ftp_client, status.CommandOk))
  // Change the level of data protection to Private
  use _ <- result.try(perform(
    ftp_client,
    command.Prot(protection_level.Private),
  ))
  use _ <- result.try(read_response(ftp_client, status.CommandOk))

  Ok(ftp_client)
}

/// Connect to a remote FTPS server using an implicit secure connection (typically port 990).
///
/// Warning: implicit FTPS is considered deprecated. Prefer explicit mode with `into_secure` when possible.
///
/// After the TLS handshake and welcome message, this function automatically sends
/// `PBSZ 0` and `PROT P` to enable data channel protection per RFC 4217.
///
/// ```gleam
/// let assert Ok(client) = gftp.connect_secure_implicit("ftp.example.com", 990, ssl_options, 30_000)
/// let assert Ok(_) = gftp.login(client, "user", "password")
/// ```
pub fn connect_secure_implicit(
  host: String,
  port: Int,
  ssl_options: kafein.WrapOptions,
  timeout: Int,
) -> FtpResult(FtpClient) {
  use socket <- result.try(
    mug.ConnectionOptions(
      host: host,
      port: port,
      timeout: timeout,
      ip_version_preference: mug.Ipv6Preferred,
    )
    |> mug.connect()
    |> result.map_error(ftp_result.ConnectionError),
  )
  use ssl_socket <- result.try(
    kafein.wrap(ssl_options, socket)
    |> result.map_error(ftp_result.Tls),
  )
  let client =
    FtpClient(
      data_stream: stream.Ssl(ssl: ssl_socket, tcp: socket),
      mode: mode.Passive,
      nat_workaround: False,
      welcome_message: None,
      passive_stream_builder: default_passive_stream_builder,
      tls_options: Some(ssl_options),
      data_timeout: default_timeout,
    )
  use client <- result.try(read_welcome_message(client))
  // Set protection buffer size
  use _ <- result.try(perform(client, command.Pbsz(0)))
  use _ <- result.try(read_response(client, status.CommandOk))
  // Change the level of data protection to Private
  use _ <- result.try(perform(client, command.Prot(protection_level.Private)))
  use _ <- result.try(read_response(client, status.CommandOk))
  Ok(client)
}

/// Get the current data stream of the FTP client. This can be either a TCP stream for plain connections or an SSL stream for secure connections.
pub fn get_stream(ftp_client: FtpClient) -> DataStream {
  ftp_client.data_stream
}

/// Log in to the FTP server with the provided username and password.
/// This is required before performing any FTP operations after connecting.
///
/// ```gleam
/// let assert Ok(client) = gftp.connect("ftp.example.com", 21)
/// let assert Ok(_) = gftp.login(client, "user", "password")
/// ```
pub fn login(
  ftp_client: FtpClient,
  username: String,
  password: String,
) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.User(username)))
  use auth_response <- result.try(
    read_response_in(ftp_client, [status.LoggedIn, status.NeedPassword]),
  )

  case auth_response.code {
    status.LoggedIn -> Ok(Nil)
    status.NeedPassword -> {
      use _ <- result.try(perform(ftp_client, command.Pass(password)))
      use _ <- result.try(read_response(ftp_client, status.LoggedIn))
      Ok(Nil)
    }
    _ -> Error(ftp_result.UnexpectedResponse(auth_response))
  }
}

/// Perform clear command channel (CCC).
/// Once the command is performed, the command channel will be encrypted no more.
/// The data stream will still be secure.
pub fn clear_command_channel(ftp_client: FtpClient) -> FtpResult(FtpClient) {
  use _ <- result.try(perform(ftp_client, command.ClearCommandChannel))
  use _ <- result.try(read_response(ftp_client, status.CommandOk))

  let ftp_client =
    FtpClient(
      ..ftp_client,
      data_stream: stream.downgrade_to_tcp(ftp_client.data_stream),
    )

  Ok(ftp_client)
}

/// Change the current working directory on the FTP server to the specified path.
///
/// ```gleam
/// let assert Ok(_) = gftp.cwd(client, "/pub/data")
/// ```
pub fn cwd(ftp_client: FtpClient, path: String) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Cwd(path)))
  use _ <- result.try(read_response(ftp_client, status.RequestedFileActionOk))

  Ok(Nil)
}

/// Change the current working directory on the FTP server to the parent directory of the current directory.
pub fn cdup(ftp_client: FtpClient) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Cdup))
  use _ <- result.try(
    read_response_in(ftp_client, [
      status.CommandOk,
      status.RequestedFileActionOk,
    ]),
  )

  Ok(Nil)
}

/// Get the current working directory on the FTP server.
///
/// ```gleam
/// let assert Ok(cwd) = gftp.pwd(client)
/// ```
pub fn pwd(ftp_client: FtpClient) -> FtpResult(String) {
  use _ <- result.try(perform(ftp_client, command.Pwd))
  use response <- result.try(read_response(ftp_client, status.PathCreated))
  use body <- result.try(utils.response_to_string(response))

  body
  |> utils.extract_str("\"")
  |> result.map_error(fn(_) { ftp_result.BadResponse })
}

/// Send a NOOP command to the FTP server to keep the connection alive or check if the server is responsive.
pub fn noop(ftp_client: FtpClient) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Noop))
  use _ <- result.try(read_response(ftp_client, status.CommandOk))

  Ok(Nil)
}

/// The EPRT command allows for the specification of an extended address for the data connection.
/// The extended address MUST consist of the network protocol as well as the network and transport addresses
pub fn eprt(
  ftp_client: FtpClient,
  address: String,
  port: Int,
  ip_version: IpVersion,
) -> FtpResult(Nil) {
  use _ <- result.try(perform(
    ftp_client,
    command.Eprt(address, port, ip_version),
  ))
  use _ <- result.try(read_response(ftp_client, status.CommandOk))

  Ok(Nil)
}

/// Create a new directory on the server at the specified path.
///
/// ```gleam
/// let assert Ok(_) = gftp.mkd(client, "new_folder")
/// ```
pub fn mkd(ftp_client: FtpClient, path: String) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Mkd(path)))
  use _ <- result.try(read_response(ftp_client, status.PathCreated))

  Ok(Nil)
}

/// Set the type of file to be transferred (FTP `TYPE` command).
///
/// Use `file_type.Binary` (or `file_type.Image`) for binary transfers,
/// `file_type.Ascii(file_type.Default)` for text transfers.
///
/// ```gleam
/// import gftp/file_type
///
/// let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
/// ```
pub fn transfer_type(
  ftp_client: FtpClient,
  file_type: file_type.FileType,
) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Type(file_type)))
  use _ <- result.try(read_response(ftp_client, status.CommandOk))

  Ok(Nil)
}

/// Quit the current FTP session.
///
/// Sends the `QUIT` command and waits for the server to confirm.
/// Call `shutdown` after this to close the underlying socket.
///
/// ```gleam
/// let assert Ok(_) = gftp.quit(client)
/// let assert Ok(_) = gftp.shutdown(client)
/// ```
pub fn quit(ftp_client: FtpClient) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Quit))
  use _ <- result.try(read_response(ftp_client, status.Closing))

  Ok(Nil)
}

/// Close the underlying socket connection to the FTP server.
///
/// You should call `quit` before this to gracefully end the FTP session.
pub fn shutdown(ftp_client: FtpClient) -> FtpResult(Nil) {
  ftp_client.data_stream
  |> stream.shutdown()
  |> result.map_error(ftp_result.Socket)
}

/// Rename a file on the FTP server.
///
/// ```gleam
/// let assert Ok(_) = gftp.rename(client, "old_name.txt", "new_name.txt")
/// ```
pub fn rename(
  ftp_client: FtpClient,
  from_name: String,
  to_name: String,
) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.RenameFrom(from_name)))
  use _ <- result.try(read_response(ftp_client, status.RequestFilePending))
  use _ <- result.try(perform(ftp_client, command.RenameTo(to_name)))
  use _ <- result.try(read_response(ftp_client, status.RequestedFileActionOk))

  Ok(Nil)
}

/// Remove the remote directory at the specified path.
///
/// ```gleam
/// let assert Ok(_) = gftp.rmd(client, "old_folder")
/// ```
pub fn rmd(ftp_client: FtpClient, path: String) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Rmd(path)))
  use _ <- result.try(read_response(ftp_client, status.RequestedFileActionOk))

  Ok(Nil)
}

/// Delete the file at the specified path on the server.
///
/// ```gleam
/// let assert Ok(_) = gftp.dele(client, "unwanted_file.txt")
/// ```
pub fn dele(ftp_client: FtpClient, path: String) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Dele(path)))
  use _ <- result.try(read_response(ftp_client, status.RequestedFileActionOk))

  Ok(Nil)
}

/// Tell the server to resume the transfer from a certain offset. The offset indicates the amount of bytes to skip
/// from the beginning of the file.
/// the REST command does not actually initiate the transfer.
/// After issuing a REST command, the client must send the appropriate FTP command to transfer the file
///
/// It is possible to cancel the REST command, sending a REST command with offset 0
pub fn rest(ftp_client: FtpClient, offset: Int) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Rest(offset)))
  use _ <- result.try(read_response(ftp_client, status.RequestFilePending))

  Ok(Nil)
}

/// Aborts a file transfer in progress.
/// This is used to cancel an ongoing file transfer operation,
/// such as a file upload or download, and close the data connection associated with that transfer.
/// 
/// This function should only be used when a file transfer is in progress, by calling it
/// from within the reader or writer function passed to `retr`, `stor` or `appe`, otherwise it may put the client in an inconsistent state.
pub fn abor(ftp_client: FtpClient) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Abor))
  use response <- result.try(
    read_response_in(ftp_client, [
      status.TransferAborted,
      status.ClosingDataConnection,
    ]),
  )
  // if the server responds with TransferAborted, we need to read the next response with ClosingDataConnection
  // to complete the abort sequence and close the data connection properly
  case response.code {
    status.TransferAborted -> {
      ftp_client
      |> read_response(status.ClosingDataConnection)
      |> result.replace(Nil)
    }
    _ -> Ok(Nil)
  }
}

/// Download a file from the FTP server.
///
/// The `reader` callback receives the data stream and should read data from it.
/// The data stream is automatically closed after the reader returns.
///
/// ```gleam
/// let assert Ok(_) = gftp.retr(client, "file.txt", fn(data_stream) {
///   let assert Ok(data) = stream.receive(data_stream, 5000)
///   // process data...
///   Ok(Nil)
/// })
/// ```
pub fn retr(
  ftp_client: FtpClient,
  path: String,
  reader: fn(DataStream) -> FtpResult(Nil),
) -> FtpResult(Nil) {
  use data_stream <- result.try(data_command(ftp_client, command.Retr(path)))
  use _ <- result.try(
    read_response_in(ftp_client, [status.AboutToSend, status.AlreadyOpen]),
  )
  // call the reader function with the data stream, which will read the file data sent by the server in response to the RETR command
  use _ <- result.try(reader(data_stream))
  // close the data stream after the reader is done
  finalize_data_command(ftp_client, data_stream)
}

/// Upload a file to the FTP server.
///
/// The `writer` callback receives the data stream and should write the file data to it.
/// The data stream is automatically closed after the writer returns.
///
/// ```gleam
/// let assert Ok(_) = gftp.stor(client, "upload.txt", fn(data_stream) {
///   stream.send(data_stream, bit_array.from_string("file contents"))
///   |> result.map_error(ftp_result.Socket)
/// })
/// ```
pub fn stor(
  ftp_client: FtpClient,
  path: String,
  writer: fn(DataStream) -> FtpResult(Nil),
) -> FtpResult(Nil) {
  use data_stream <- result.try(data_command(ftp_client, command.Stor(path)))
  use _ <- result.try(
    read_response_in(ftp_client, [status.AboutToSend, status.AlreadyOpen]),
  )
  // call the writer function with the data stream, which will write the file data to the server in response to the STOR command
  use _ <- result.try(writer(data_stream))
  // close the data stream after the writer is done
  finalize_data_command(ftp_client, data_stream)
}

/// Append data to a file on the FTP server. If the file doesn't exist, it will be created.
///
/// ```gleam
/// let assert Ok(_) = gftp.appe(client, "log.txt", fn(data_stream) {
///   stream.send(data_stream, bit_array.from_string("new log entry\n"))
///   |> result.map_error(ftp_result.Socket)
/// })
/// ```
pub fn appe(
  ftp_client: FtpClient,
  path: String,
  writer: fn(DataStream) -> FtpResult(Nil),
) -> FtpResult(Nil) {
  use data_stream <- result.try(data_command(ftp_client, command.Appe(path)))
  use _ <- result.try(
    read_response_in(ftp_client, [status.AboutToSend, status.AlreadyOpen]),
  )
  // call the writer function with the data stream, which will write the file data to the server in response to the STOR command
  use _ <- result.try(writer(data_stream))
  // close the data stream after the writer is done
  finalize_data_command(ftp_client, data_stream)
}

/// Execute `LIST` command which returns the detailed file listing in human readable format.
/// If `pathname` is omitted then the list of files in the current directory will be
/// returned otherwise it will the list of files on `pathname`.
///
/// ### Parse result
///
/// You can parse the output of this command with `gftp/list` module, which provides a `File` type
/// and the `parse_list` function to parse the output of the `LIST` command into a list of `File` structs,
/// which contain structured information about each file in the listing, such as name, size, permissions, and modification date.
///
/// ```gleam
/// import gftp
/// import gftp/list as gftp_list
/// import gleam/list
///
/// use lines <- result.try(gftp.list(ftp_client, None))
/// lines
/// |> list.try_map(gftp_list.parse_list)
/// ```
pub fn list(
  ftp_client: FtpClient,
  pathname: Option(String),
) -> FtpResult(List(String)) {
  stream_lines(ftp_client, command.List(pathname), status.AboutToSend)
}

/// Execute `NLST` command which returns the list of file names only.
/// If `pathname` is omitted then the list of files in the current directory will be
/// returned otherwise it will the list of files on `pathname`.
///
/// ### Parse result
///
/// The output of the `NLST` command is just a list of file names, so it doesn't require any special parsing like the `LIST` command.
pub fn nlst(
  ftp_client: FtpClient,
  pathname: Option(String),
) -> FtpResult(List(String)) {
  stream_lines(ftp_client, command.Nlst(pathname), status.AboutToSend)
}

/// Execute `MLSD` command which returns the machine-processable listing of a directory.
/// If `pathname` is omitted then the list of files in the current directory will be
/// returned otherwise it will the list of files on `pathname`.
///
/// ### Parse result
///
/// The output of the `MLSD` command is a machine-readable format that provides detailed information about each file in the listing,
/// such as name, size, permissions, and modification date, in a standardized format defined by RFC 3659.
///
/// You can parse the output of this command with `gftp/list` module, which provides a `File` type and the `parse_mlsd`
/// function to parse the output of the `MLSD` command into a list of `File` structs.
///
/// ```gleam
/// import gftp
/// import gftp/list as gftp_list
/// import gleam/list
///
/// use lines <- result.try(gftp.mlsd(ftp_client, None))
/// lines
/// |> list.try_map(gftp_list.parse_mlsd)
/// ```
pub fn mlsd(
  ftp_client: FtpClient,
  pathname: Option(String),
) -> FtpResult(List(String)) {
  stream_lines(ftp_client, command.Mlsd(pathname), status.AboutToSend)
}

/// Execute `MLST` command which returns the machine-processable information for a file
/// 
/// ### Parse result
/// 
/// The output of the `MLST` command is a machine-readable format that provides detailed information about a file,
/// such as name, size, permissions, and modification date, in a standardized format defined by RFC 3659.
/// 
/// You can parse the output of this command with `gftp/list` module, which provides a `File` type and the `parse_mlst`
/// function to parse the output of the `MLST` command into a `File` struct.
/// 
/// ```gleam
/// import gftp
/// import gftp/list as gftp_list
/// import gleam/list
/// 
/// use line <- result.try(gftp.mlst(ftp_client, None))
/// use file <- result.try(gftp_list.parse_mlst(line))
/// ```
pub fn mlst(
  ftp_client: FtpClient,
  pathname: Option(String),
) -> FtpResult(String) {
  use _ <- result.try(perform(ftp_client, command.Mlst(pathname)))
  use response <- result.try(read_response(
    ftp_client,
    status.RequestedFileActionOk,
  ))
  use body <- result.try(utils.response_to_string(response))
  // read line 1 (not 0, 1)
  body
  |> string.split("\r\n")
  |> list.drop(1)
  |> list.first
  |> result.map_error(fn(_) { ftp_result.BadResponse })
}

/// Retrieve the modification time of the file at `pathname`.
///
/// ```gleam
/// let assert Ok(mtime) = gftp.mdtm(client, "file.txt")
/// ```
pub fn mdtm(ftp_client: FtpClient, pathname: String) -> FtpResult(Timestamp) {
  let assert Ok(regex) = regexp.from_string(mdtm_regex)
  use _ <- result.try(perform(ftp_client, command.Mdtm(pathname)))
  use response <- result.try(read_response(ftp_client, status.File))
  use body <- result.try(utils.response_to_string(response))

  case utils.re_matches(regex, body) {
    Ok([
      Some(year),
      Some(month),
      Some(day),
      Some(hours),
      Some(minutes),
      Some(seconds),
    ]) -> {
      use year <- result.try(utils.parse_int(year))
      use month <- result.try(utils.parse_month(month))
      use day <- result.try(utils.parse_int(day))
      use hours <- result.try(utils.parse_int(hours))
      use minutes <- result.try(utils.parse_int(minutes))
      use seconds <- result.try(utils.parse_int(seconds))

      Ok(timestamp.from_calendar(
        date: calendar.Date(year: year, month: month, day: day),
        time: calendar.TimeOfDay(
          hours: hours,
          minutes: minutes,
          seconds: seconds,
          nanoseconds: 0,
        ),
        offset: duration.new(0, duration.Second),
      ))
    }
    _ -> Error(ftp_result.BadResponse)
  }
}

/// Retrieve the size in bytes of the file at `pathname`.
///
/// ```gleam
/// let assert Ok(size) = gftp.size(client, "file.txt")
/// ```
pub fn size(ftp_client: FtpClient, pathname: String) -> FtpResult(Int) {
  let assert Ok(regex) = regexp.from_string(size_regex)
  use _ <- result.try(perform(ftp_client, command.Size(pathname)))
  use response <- result.try(read_response(ftp_client, status.File))
  use body <- result.try(utils.response_to_string(response))

  case utils.re_matches(regex, body) {
    Ok([Some(size)]) -> utils.parse_int(size)
    _ -> Error(ftp_result.BadResponse)
  }
}

/// Retrieve the features supported by the server (RFC 2389 FEAT command).
///
/// Returns a dictionary of feature names to optional parameter strings.
///
/// ```gleam
/// import gleam/dict
///
/// let assert Ok(features) = gftp.feat(client)
/// let supports_mlst = dict.has_key(features, "MLST")
/// ```
pub fn feat(ftp_client: FtpClient) -> FtpResult(Features) {
  use _ <- result.try(perform(ftp_client, command.Feat))
  use response <- result.try(read_response(ftp_client, status.System))
  use body <- result.try(utils.response_to_string(response))

  let lines = string.split(body, "\n")
  case lines {
    // Single line response (no features)
    [single] ->
      case feat.is_last_line(single) {
        True -> Ok(dict.new())
        False -> Error(ftp_result.BadResponse)
      }
    // Multiline: drop first (header) and last (closing "211 End") lines
    [_, ..rest] -> {
      let feature_lines =
        rest
        |> list.reverse
        |> list.drop(1)
        |> list.reverse
      feat.parse_features(feature_lines)
    }
    [] -> Ok(dict.new())
  }
}

/// Set a server option (RFC 2389 OPTS command).
///
/// ```gleam
/// import gleam/option.{Some}
///
/// let assert Ok(_) = gftp.opts(client, "UTF8", Some("ON"))
/// ```
pub fn opts(
  ftp_client: FtpClient,
  option: String,
  value: Option(String),
) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Opts(option, value)))
  use _ <- result.try(read_response(ftp_client, status.CommandOk))

  Ok(Nil)
}

/// Execute a SITE command on the server and return the response.
///
/// ```gleam
/// let assert Ok(response) = gftp.site(client, "CHMOD 755 file.txt")
/// ```
pub fn site(ftp_client: FtpClient, sub_command: String) -> FtpResult(Response) {
  use _ <- result.try(perform(ftp_client, command.Site(sub_command)))
  read_response_in(ftp_client, [status.CommandOk, status.Help])
}

/// Execute a custom FTP command and return the response.
///
/// Provide a list of expected status codes to validate the response against.
///
/// ```gleam
/// import gftp/status
///
/// let assert Ok(response) = gftp.custom_command(client, "SITE HELP", [status.Help])
/// ```
pub fn custom_command(
  ftp_client: FtpClient,
  command_str: String,
  expected_statuses: List(Status),
) -> FtpResult(Response) {
  use _ <- result.try(perform(ftp_client, command.Custom(command_str)))
  read_response_in(ftp_client, expected_statuses)
}

/// Execute a custom FTP command that uses a data connection.
///
/// The `on_data_stream` callback receives the data stream and the server response.
/// Use `read_lines_from_stream` to easily parse line-based output.
///
/// ```gleam
/// import gftp/status
///
/// let assert Ok(_) = gftp.custom_data_command(
///   client,
///   "LIST -la",
///   [status.AboutToSend, status.AlreadyOpen],
///   fn(data_stream, _response) {
///     let assert Ok(lines) = gftp.read_lines_from_stream(data_stream, 5000)
///     Ok(Nil)
///   },
/// )
/// ```
pub fn custom_data_command(
  ftp_client: FtpClient,
  command_str: String,
  expected_statuses: List(Status),
  on_data_stream: fn(DataStream, Response) -> FtpResult(Nil),
) -> FtpResult(Nil) {
  use data_stream <- result.try(data_command(
    ftp_client,
    command.Custom(command_str),
  ))
  use response <- result.try(read_response_in(ftp_client, expected_statuses))
  // call the provided function with the data stream, which will read/write the data sent by the server in response to the custom command
  use _ <- result.try(on_data_stream(data_stream, response))
  // close the data stream after the provided function is done
  finalize_data_command(ftp_client, data_stream)
}

/// Read lines from a data stream until the stream is closed by the server or an error occurs.
pub fn read_lines_from_stream(
  data_stream: DataStream,
  timeout: Int,
) -> FtpResult(List(String)) {
  stream.set_line_mode(data_stream)
  read_lines_from_stream_loop([], data_stream, timeout)
}

// -------------------------------
// !internal functions
// -------------------------------

/// Read the server's welcome message (status 220 Ready) and return
/// an updated FtpClient with the welcome_message field populated.
fn read_welcome_message(client: FtpClient) -> FtpResult(FtpClient) {
  stream.set_line_mode(client.data_stream)
  case read_response(client, status.Ready) {
    Ok(resp) -> {
      let welcome_msg = case response.to_string(resp) {
        Ok(msg) -> Some(msg)
        Error(_) -> None
      }
      Ok(FtpClient(..client, welcome_message: welcome_msg))
    }
    Error(err) -> Error(err)
  }
}

/// Key function to read a response from the server and check if it matches the expected status code.
/// 
/// It returns error if it fails to read a response or if the response status code doesn't match the expected status.
/// 
/// It internally just calls `read_response_in` with a single expected status code
fn read_response(
  ftp_client: FtpClient,
  expected_status: Status,
) -> FtpResult(Response) {
  read_response_in(ftp_client, [expected_status])
}

/// Read a response from the server and check if it matches any of the expected status codes.
///
/// **WARNING:** This function is intended for internal use by `gftp/actor` and
/// `gftp/internal/data_channel`. It may change without notice in future versions.
pub fn read_response_in(
  ftp_client: FtpClient,
  expected_statuses: List(Status),
) -> FtpResult(Response) {
  use line <- result.try(read_line(ftp_client))
  use status <- result.try(status_from_bytes(line))

  // In FTP, single-line responses have a space as the 4th character (e.g. "220 Ok"),
  // while multi-line responses use a dash (e.g. "220-First line").
  let is_multiline = string.slice(line, 3, 1) == "-"

  use body <- result.try(case is_multiline {
    False -> Ok([line])
    True -> {
      let first_3_bytes = line |> string.slice(0, 3) |> bit_array.from_string
      let final_line = bit_array.append(first_3_bytes, <<0x20>>)
      let expected = case list.contains(expected_statuses, status.System) {
        True -> [bit_array.append(first_3_bytes, <<"-":utf8>>), final_line]
        False -> [final_line]
      }
      read_response_body([line], ftp_client, expected)
    }
  })

  let body =
    body
    |> string.join("\n")
    |> bit_array.from_string
  let response = Response(status, body)

  case list.contains(expected_statuses, status) {
    True -> Ok(response)
    False -> Error(ftp_result.UnexpectedResponse(response))
  }
}

/// Read a line from the control connection stream and return it as a string along with bytes read.
fn read_line(ftp_client: FtpClient) -> FtpResult(String) {
  read_line_with(ftp_client.data_stream, ftp_client.data_timeout)
}

/// Read a line from the provided data stream with the specified timeout and return it as a string.
/// Expects the stream to be in {packet, line} mode, where recv returns one complete line per call.
fn read_line_with(data_stream: DataStream, timeout: Int) -> FtpResult(String) {
  case stream.receive(data_stream, timeout) {
    Ok(bytes) -> {
      let bytes = strip_trailing_lf(bytes)
      bytes
      |> bit_array.to_string
      |> result.map_error(fn(_) { ftp_result.BadResponse })
    }
    Error(e) -> Error(ftp_result.Socket(e))
  }
}

/// Strip a trailing 0x0A (LF) byte if present, since {packet, line} includes it.
fn strip_trailing_lf(bytes: BitArray) -> BitArray {
  let size = bit_array.byte_size(bytes)
  case size > 0 {
    True ->
      case bit_array.slice(bytes, size - 1, 1) {
        Ok(<<0x0A>>) -> {
          let assert Ok(trimmed) = bit_array.slice(bytes, 0, size - 1)
          trimmed
        }
        _ -> bytes
      }
    False -> bytes
  }
}

/// Read the response body from the server, handling multi-line responses if necessary.
fn read_response_body(
  acc: List(String),
  ftp_client: FtpClient,
  expected: List(BitArray),
) -> FtpResult(List(String)) {
  use line <- result.try(read_line(ftp_client))

  case
    string.length(line) < 5
    || !list.contains(
      expected,
      line |> string.slice(0, 4) |> bit_array.from_string,
    )
  {
    True -> read_response_body([line, ..acc], ftp_client, expected)
    False -> [line, ..acc] |> list.reverse |> Ok
  }
}

/// Read the `Status` code from the response line and convert it to a `Status` type.
fn status_from_bytes(line: String) -> FtpResult(Status) {
  line
  |> string.slice(0, 3)
  |> int.parse
  |> result.map(status.from_int)
  |> result.map_error(fn(_) { ftp_result.BadResponse })
}

/// Write data to stream with the command to perform
fn perform(ftp_client: FtpClient, command: Command) -> FtpResult(Nil) {
  command
  |> command.to_string
  |> fn(s) { s <> "\r\n" }
  |> bit_array.from_string
  |> fn(data) { stream.send(ftp_client.data_stream, data) }
  |> result.map_error(ftp_result.Socket)
}

/// Default implementation of the `PassiveStreamBuilder` function type.
/// 
/// It just connects to the specified host and port using a TCP stream and wraps it in a `DataStream` type.
fn default_passive_stream_builder(
  host: String,
  port: Int,
) -> FtpResult(DataStream) {
  mug.new(host, port)
  |> mug.connect()
  |> result.map_error(ftp_result.ConnectionError)
  |> result.map(fn(socket) { stream.Tcp(socket) })
}

/// Execute a command that uses a separate data stream.
///
/// **WARNING:** This function is intended for internal use by `gftp/actor` and
/// `gftp/internal/data_channel`. It may change without notice in future versions.
pub fn data_command(
  ftp_client: FtpClient,
  command: Command,
) -> FtpResult(DataStream) {
  case ftp_client.mode {
    mode.Active(timeout) -> data_command_active(ftp_client, command, timeout)
    mode.Passive -> data_command_passive(ftp_client, command)
    mode.ExtendedPassive -> data_command_extended_passive(ftp_client, command)
  }
}

/// Execute a data command in active mode, which requires the client to listen for an incoming connection from the server for the data transfer.
fn data_command_active(
  ftp_client: FtpClient,
  command: Command,
  timeout: Int,
) -> FtpResult(DataStream) {
  // Get the local IP of the control connection
  use #(local_ip, _) <- result.try(
    stream.local_address(ftp_client.data_stream)
    |> result.map_error(ftp_result.Socket),
  )
  let is_ipv6 = utils.is_ipv6_address(local_ip)
  // Create a TCP listener on an ephemeral port with the matching address family
  let ip_family = case is_ipv6 {
    True -> listener.Ipv6
    False -> listener.Ipv4
  }
  use listen_socket <- result.try(
    listener.listen(ip_family)
    |> result.map_error(ftp_result.Socket),
  )
  // Get the assigned port
  use listen_port <- result.try(
    listener.port(listen_socket)
    |> result.map_error(ftp_result.Socket),
  )
  // send PORT or EPRT command and read response
  use _ <- result.try(send_port_command(
    ftp_client,
    local_ip,
    listen_port,
    is_ipv6,
  ))
  use _ <- result.try(read_response(ftp_client, status.CommandOk))
  // Send the actual data command (RETR, STOR, etc.)
  use _ <- result.try(perform(ftp_client, command))
  // Accept the incoming connection from the server
  let accept_result =
    listener.accept(listen_socket, timeout)
    |> result.map_error(ftp_result.Socket)
  // Always close the listener
  listener.close(listen_socket)
  use accepted_socket <- result.try(accept_result)
  // Wrap as a DataStream
  let data_stream = stream.Tcp(accepted_socket)
  // Upgrade to TLS if configured
  case ftp_client.tls_options {
    Some(ssl_options) ->
      data_stream
      |> stream.upgrade_to_ssl(ssl_options)
      |> result.map_error(ftp_result.Tls)
    None -> Ok(data_stream)
  }
}

/// Send the appropriate PORT or EPRT command to the server based on
/// the IP version of the local address.
fn send_port_command(
  ftp_client: FtpClient,
  local_ip: String,
  listen_port: Int,
  is_ipv6: Bool,
) -> FtpResult(Nil) {
  case is_ipv6 {
    True -> perform(ftp_client, command.Eprt(local_ip, listen_port, mode.V6))
    False -> {
      let port_string = build_active_port_arg(local_ip, listen_port)
      perform(ftp_client, command.Port(port_string))
    }
  }
}

/// Build the PORT command argument string from an IP address and port.
/// Format: "h1,h2,h3,h4,p1,p2" where h1-h4 are IP octets and p1,p2 are port MSB,LSB.
fn build_active_port_arg(ip: String, port: Int) -> String {
  let ip_part = string.replace(ip, ".", ",")
  let msb = port / 256
  let lsb = port % 256
  ip_part <> "," <> int.to_string(msb) <> "," <> int.to_string(lsb)
}

/// Execute a data command in passive mode, which requires the client to connect to the server at the specified address
/// and port for the data transfer.
fn data_command_passive(
  ftp_client: FtpClient,
  command: Command,
) -> FtpResult(DataStream) {
  use _ <- result.try(perform(ftp_client, command.Pasv))
  use response <- result.try(read_response(ftp_client, status.PassiveMode))
  use #(address, port) <- result.try(parse_passive_address_from_response(
    response,
  ))
  use _ <- result.try(perform(ftp_client, command))

  use #(address, port) <- result.try(case ftp_client.nat_workaround {
    False -> Ok(#(address, port))
    True -> {
      ftp_client.data_stream
      |> stream.peer_address
      |> result.map(fn(peer_addr) { #(peer_addr.0, port) })
      |> result.map_error(ftp_result.Socket)
    }
  })

  build_data_channel_stream(ftp_client, address, port)
}

/// Execute a data command in extended passive mode, which requires the client to connect to the server at the specified address
/// and port for the data transfer.
fn data_command_extended_passive(
  ftp_client: FtpClient,
  command: Command,
) -> FtpResult(DataStream) {
  use _ <- result.try(perform(ftp_client, command.Epsv))
  use response <- result.try(read_response(
    ftp_client,
    status.ExtendedPassiveMode,
  ))
  use port <- result.try(parse_epsv_address_from_response(response))
  use _ <- result.try(perform(ftp_client, command))
  use #(address, _) <- result.try(
    ftp_client.data_stream
    |> stream.peer_address
    |> result.map_error(ftp_result.Socket),
  )

  build_data_channel_stream(ftp_client, address, port)
}

/// Parse the passive mode address and port from the server's response to the `PASV` command.
fn parse_passive_address_from_response(
  response: Response,
) -> FtpResult(#(String, Int)) {
  let assert Ok(regex) = regexp.from_string(pasv_port_regex)
  use response <- result.try(utils.response_to_string(response))
  case utils.re_matches(regex, response) {
    Ok([Some(a), Some(b), Some(c), Some(d), Some(msb), Some(lsb)]) -> {
      let address = string.join([a, b, c, d], ".")
      use msb <- result.try(
        msb
        |> int.parse
        |> result.map_error(fn(_) { ftp_result.BadResponse }),
      )
      use lsb <- result.try(
        lsb
        |> int.parse
        |> result.map_error(fn(_) { ftp_result.BadResponse }),
      )

      let port =
        msb
        |> int.bitwise_shift_left(8)
        |> int.bitwise_or(lsb)
      Ok(#(address, port))
    }
    _ -> Error(ftp_result.BadResponse)
  }
}

/// Parse the extended passive mode port from the server's response to the `EPSV` command.
fn parse_epsv_address_from_response(response: Response) -> FtpResult(Int) {
  let assert Ok(regex) = regexp.from_string(epsv_port_regex)
  use response <- result.try(utils.response_to_string(response))

  case utils.re_matches(regex, response) {
    Ok([Some(port_str)]) ->
      port_str
      |> int.parse
      |> result.map_error(fn(_) { ftp_result.BadResponse })
    _ -> Error(ftp_result.BadResponse)
  }
}

/// Build a data stream for the data connection in passive mode, using the provided host and port.
/// 
/// If TLS options are set in the `FtpClient`, it will upgrade the stream to a secure connection using SSL/TLS before returning it.
fn build_data_channel_stream(
  ftp_client: FtpClient,
  host: String,
  port: Int,
) -> FtpResult(DataStream) {
  use stream <- result.try(ftp_client.passive_stream_builder(host, port))

  case ftp_client.tls_options {
    Some(ssl_options) ->
      stream
      |> stream.upgrade_to_ssl(ssl_options)
      |> result.map_error(ftp_result.Tls)
    None -> Ok(stream)
  }
}

/// Finalize a data command by closing the data stream and reading the server's final response.
///
/// **WARNING:** This function is intended for internal use by `gftp/actor` and
/// `gftp/internal/data_channel`. It may change without notice in future versions.
pub fn finalize_data_command(
  ftp_client: FtpClient,
  data_stream: DataStream,
) -> FtpResult(Nil) {
  // Close the data stream after the reader/writer is done.
  // Ignore shutdown errors (e.g. Enotconn) since the server may have already
  // closed the data connection, which is normal FTP behavior after data transfer.
  let _ = stream.shutdown(data_stream)
  // read the final response from the server after the data transfer is complete
  read_response_in(ftp_client, [
    status.ClosingDataConnection,
    status.RequestedFileActionOk,
  ])
  |> result.replace(Nil)
}

/// Execute a data command which returns list of strings in a separate stream.
/// 
/// This is basically a convenience function to execute a data command and read the response as a list of lines,
/// which is useful for commands like `LIST` or `NLST` that return directory listings.
fn stream_lines(
  ftp_client: FtpClient,
  command: Command,
  open_code: Status,
) -> FtpResult(List(String)) {
  use data_stream <- result.try(data_command(ftp_client, command))
  use _ <- result.try(
    read_response_in(ftp_client, [open_code, status.AlreadyOpen]),
  )
  use lines <- result.try(read_lines_from_stream(
    data_stream,
    ftp_client.data_timeout,
  ))
  use _ <- result.try(finalize_data_command(ftp_client, data_stream))

  Ok(lines)
}

/// Recursive loop for `read_lines_from_stream`
fn read_lines_from_stream_loop(
  acc: List(String),
  data_stream: DataStream,
  timeout: Int,
) -> FtpResult(List(String)) {
  case read_line_with(data_stream, timeout) {
    Ok(line) -> read_lines_from_stream_loop([line, ..acc], data_stream, timeout)
    Error(ftp_result.Socket(mug.Closed)) -> acc |> list.reverse |> Ok
    Error(e) -> Error(e)
  }
}
