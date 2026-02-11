//// GFTP - A ✨ Gleam FTP/FTPS client library with full RFC support for both passive and active mode
//// 
//// Gleam FTP (gftp) is Gleam Client library for FTP (File Transfer Protocol) and FTPS (FTP over SSL/TLS) with full
//// RFC 959, RFC 2228, RFC 4217, RFC 2428 and RFC 2389 compliance.
//// 
//// ## Naming of functions
//// 
//// The `FtpClient` function names follow the FTP command names as much as possible,
//// so if you are familiar with FTP commands, you will find it easy to use gftp.
//// Otherwise I can understand that `dele` instead of `delete` can be a bit confusing,
//// but I wanted to keep the command names as close as possible to the actual FTP commands for better 
//// readability and easier mapping to the FTP protocol.
//// 
//// ## Add gftp to your project
//// 
//// ```bash
//// gleam add gftp@1
//// ```
//// 
//// ## Usage
////
//// ```gleam
//// import gftp
//// import gftp/command/file_type
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
////   let assert Ok(entries) = gftp.list(client, None)
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

import gftp/command.{type Command}
import gftp/command/feat.{type Features}
import gftp/command/file_type
import gftp/command/protection_level
import gftp/mode.{type Mode}
import gftp/response.{type Response, Response}
import gftp/result.{type FtpResult} as ftp_result
import gftp/status.{type Status}
import gftp/stream.{type DataStream}
import gftp/utils
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

/// Try to connect to the remote server
/// 
/// The default timeout of 30 seconds is used for the connection attempt.
/// 
/// This method connects to the FTP server at the specified
/// `host` and `port`, reads the server's welcome message, and initializes the `FtpClient` instance with the appropriate settings.
/// 
/// This method doesn't authenticate with the server, so after a successful connection, you will need to call the `login`
/// method to authenticate before performing any FTP operations.
/// 
/// On success, returns a `FtpClient` instance that can be used to interact with the FTP server.
/// On failure, returns an `FtpError` describing the issue.
pub fn connect(host: String, port: Int) -> FtpResult(FtpClient) {
  // Default timeout of 30 seconds
  connect_timeout(host, port, timeout: default_timeout)
}

/// Try to connect to the remote server
/// 
/// The `timeout` parameter specifies the maximum time to wait for a connection to be established, in milliseconds.
/// 
///  This method connects to the FTP server at the specified
/// `host` and `port`, reads the server's welcome message, and initializes the `FtpClient` instance with the appropriate settings.
/// 
/// This method doesn't authenticate with the server, so after a successful connection, you will need to call the `login`
/// method to authenticate before performing any FTP operations.
/// 
/// On success, returns a `FtpClient` instance that can be used to interact with the FTP server.
/// On failure, returns an `FtpError` describing the issue.
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
  |> result.try(fn(socket) { connect_with_stream(socket, host, port) })
}

/// Connect to the FTP server using an existing socket stream.
/// 
/// This method doesn't authenticate with the server, so after a successful connection, you will need to call the `login`
/// method to authenticate before performing any FTP operations.
/// 
/// On success, returns a `FtpClient` instance that can be used to interact with the FTP server.
/// On failure, returns an `FtpError` describing the issue.
pub fn connect_with_stream(
  stream: mug.Socket,
  host: String,
  port: Int,
) -> FtpResult(FtpClient) {
  let client =
    FtpClient(
      data_stream: stream.Tcp(stream, host: host, port: port),
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

/// Enable active mode for the FTP client with the specified timeout for the data connection.
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

/// Set data mode
pub fn with_mode(ftp_client: FtpClient, mode: Mode) -> FtpClient {
  FtpClient(..ftp_client, mode: mode)
}

/// Set NAT workaround for passive mode
pub fn with_nat_workaround(
  ftp_client: FtpClient,
  nat_workaround: Bool,
) -> FtpClient {
  FtpClient(..ftp_client, nat_workaround: nat_workaround)
}

/// Switch to explicit secure mode if possible (FTPS), using a provided SSL configuration.
/// This method does nothing if the connect is already secured.
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

/// Connect to remote ftps server using IMPLICIT secure connection.
/// 
/// Warning: mind that implicit ftps should be considered deprecated, if you can use explicit mode with [`into_secure`]
/// Most of modern servers don't support implicit ftps anymore, but if you need to connect to one of those legacy servers,
/// this function can be useful.
pub fn connect_secure_implicit(
  host: String,
  port: Int,
  ssl_options: kafein.WrapOptions,
  timeout: Int,
) -> FtpResult(FtpClient) {
  mug.ConnectionOptions(
    host: host,
    port: port,
    timeout: timeout,
    ip_version_preference: mug.Ipv6Preferred,
  )
  |> mug.connect()
  |> result.map_error(ftp_result.ConnectionError)
  |> result.try(fn(socket) {
    case kafein.wrap(ssl_options, socket) {
      Ok(ssl_socket) -> {
        let client =
          FtpClient(
            data_stream: stream.Ssl(
              ssl: ssl_socket,
              tcp: socket,
              host: host,
              port: port,
            ),
            mode: mode.Passive,
            nat_workaround: False,
            welcome_message: None,
            passive_stream_builder: default_passive_stream_builder,
            tls_options: Some(ssl_options),
            data_timeout: default_timeout,
          )
        read_welcome_message(client)
      }
      Error(e) -> Error(ftp_result.Tls(e))
    }
  })
}

/// Get the current data stream of the FTP client. This can be either a TCP stream for plain connections or an SSL stream for secure connections.
pub fn get_stream(ftp_client: FtpClient) -> DataStream {
  ftp_client.data_stream
}

/// Log in to the FTP server with the provided username and password.
/// This is required before performing any FTP operations after connecting to the server.
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
  ip_version: command.IpVersion,
) -> FtpResult(Nil) {
  use _ <- result.try(perform(
    ftp_client,
    command.Eprt(address, port, ip_version),
  ))
  use _ <- result.try(read_response(ftp_client, status.CommandOk))

  Ok(Nil)
}

/// This creates a new directory on the server at the specified path.
pub fn mkd(ftp_client: FtpClient, path: String) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Mkd(path)))
  use _ <- result.try(read_response(ftp_client, status.PathCreated))

  Ok(Nil)
}

/// Sets the type of file to be transferred. That is the implementation of the `TYPE` command,
/// which is used to specify the type of file being transferred (e.g., ASCII, binary).
pub fn transfer_type(
  ftp_client: FtpClient,
  file_type: file_type.FileType,
) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Type(file_type)))
  use _ <- result.try(read_response(ftp_client, status.CommandOk))

  Ok(Nil)
}

/// Quits the current FTP session.
/// 
/// This doesn't shutdown the data stream immediately, but it sends the `QUIT` command to the server and waits for the
/// server to respond with a closing connection status before returning.
pub fn quit(ftp_client: FtpClient) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Quit))
  use _ <- result.try(read_response(ftp_client, status.Closing))

  Ok(Nil)
}

/// Shutdown the data stream of the FTP client. This is used to close the connection to the FTP server.
pub fn shutdown(ftp_client: FtpClient) -> FtpResult(Nil) {
  ftp_client.data_stream
  |> stream.shutdown()
  |> result.map_error(ftp_result.Socket)
}

/// Renames the file from_name to to_name on the FTP server.
/// This is a two-step process where you first specify the file to rename with `RenameFrom` and then specify the new name with `RenameTo`.
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

/// Removes the remote directory specified by the path from the server.
pub fn rmd(ftp_client: FtpClient, path: String) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Rmd(path)))
  use _ <- result.try(read_response(ftp_client, status.RequestedFileActionOk))

  Ok(Nil)
}

/// Deletes the file specified by the path from the server.
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

  ftp_client
  |> read_response_in([
    status.TransferAborted,
    status.ClosingDataConnection,
  ])
  |> result.replace(Nil)
}

/// Retrieves a file from the FTP server at the specified path and processes it with the provided reader function.
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

/// Stores a file on the FTP server at the specified path by sending the file data through the provided writer function.
/// 
/// This is used to upload a file to the FTP server.
/// The `writer` function is called with a `DataStream` that can be used to write the file data to the server in response to the STOR command.
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

/// Appends a file on the FTP server at the specified path by sending the file data through the provided writer function.
/// 
/// This is used to append data to an existing file on the FTP server. If the file doesn't exist, it will be created.
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
/// If `pathname` is omited then the list of files in the current directory will be
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
/// If `pathname` is omited then the list of files in the current directory will be
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
/// If `pathname` is omited then the list of files in the current directory will be
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
/// function to parse the output of the `MLSD` command into a list of `File` structs.
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

/// Retrieves the modification time of the file at `pathname` if it exists.
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

/// Retrieves the size of the file at `pathname` if it exists.
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

/// Retrieves the features supported by the server, through the FEAT command.
pub fn feat(ftp_client: FtpClient) -> FtpResult(Features) {
  use _ <- result.try(perform(ftp_client, command.Feat))
  use response <- result.try(read_response(ftp_client, status.CommandOk))
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

/// Set option `option` with an optional value by performing the OPTS command.
pub fn opts(
  ftp_client: FtpClient,
  option: String,
  value: Option(String),
) -> FtpResult(Nil) {
  use _ <- result.try(perform(ftp_client, command.Opts(option, value)))
  use _ <- result.try(read_response(ftp_client, status.CommandOk))

  Ok(Nil)
}

/// Execute a command on the server and return the response
pub fn site(ftp_client: FtpClient, sub_command: String) -> FtpResult(Response) {
  use _ <- result.try(perform(ftp_client, command.Site(sub_command)))
  read_response(ftp_client, status.CommandOk)
}

/// Perform custom command on the server and return the response
/// 
/// Provide a list of expected status codes to check the response against.
pub fn custom_command(
  ftp_client: FtpClient,
  command_str: String,
  expected_statuses: List(Status),
) -> FtpResult(Response) {
  use _ <- result.try(perform(ftp_client, command.Custom(command_str)))
  read_response_in(ftp_client, expected_statuses)
}

/// Perform a custom command using the data connection.
/// It allows you to execute a custom command that involves a data connection,
/// by providing a function that will be called with the data stream and the response from the server after the command is executed.
///
/// If you want you can easily parse lines from the [`DataStream`] using [`read_lines_from_stream`].
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

/// Key function to read a response from the server and check if it matches any of the expected status codes.
/// 
/// It returns error if it fails to read a response or if the response status code doesn't match any of the expected statuses.
fn read_response_in(
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
  Ok(Response(status, body))
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
  |> result.map(fn(socket) { stream.Tcp(socket, host, port) })
}

/// Execute command which send data back in a separate stream.
/// 
/// On success, it returns a `DataStream` that can be used to read the data sent by the server in response to the command.
fn data_command(
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
  _ftp_client: FtpClient,
  _command: Command,
  _timeout: Int,
) -> FtpResult(DataStream) {
  todo as "Requires additional libs"
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

  let #(address, port) = case ftp_client.nat_workaround {
    False -> #(address, port)
    True -> {
      ftp_client.data_stream
      |> stream.socket_address
      |> fn(socket_addr) { #(socket_addr.0, port) }
    }
  }

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
  let address = stream.socket_address(ftp_client.data_stream).0

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

/// Finalize a data command by closing the data stream and reading the final response from the server after the data transfer is complete.
fn finalize_data_command(
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
