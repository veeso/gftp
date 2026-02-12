//// OTP actor wrapper for gftp that serializes all FTP operations.
////
//// Wrapping an `FtpClient` in an actor prevents protocol state corruption
//// by ensuring that control commands are rejected while a data channel is open.
////
//// ## Usage
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
//// let assert Ok(cwd) = ftp_actor.pwd(handle)
//// let assert Ok(_) = ftp_actor.quit(handle)
//// ```
////
//// ## Message-based streaming
////
//// The actor enforces chunk protection: once a data channel is opened via
//// `open_retr`, `open_stor`, etc., all control commands return
//// `Error(DataTransferInProgress)` until `close_data_channel` is called.
////
//// ```gleam
//// let assert Ok(data_stream) = ftp_actor.open_retr(handle, "file.txt")
//// // ... receive data via stream.receive_next_packet_as_message ...
//// let assert Ok(_) = ftp_actor.close_data_channel(handle, data_stream)
//// ```

import gftp.{type FtpClient, type PassiveStreamBuilder}
import gftp/file_type
import gftp/internal/command
import gftp/internal/command/feat.{type Features}
import gftp/internal/data_channel
import gftp/mode.{type Mode}
import gftp/response.{type Response}
import gftp/result.{type FtpResult} as ftp_result
import gftp/status.{type Status}
import gftp/stream.{type DataStream}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}
import gleam/otp/actor
import gleam/time/timestamp.{type Timestamp}
import kafein

/// Default timeout for actor calls in milliseconds (30 seconds).
const default_call_timeout = 30_000

/// Actor state holding the FTP client and a chunk flag.
/// When `chunk` is `True`, a data channel is open and control commands are rejected.
type State {
  State(client: FtpClient, chunk: Bool)
}

/// Handle to an FTP actor. Use the public functions in this module to interact with it.
pub type Handle =
  Subject(Message)

/// Internal message type for the FTP actor.
/// Users interact via the public functions, not by sending messages directly.
pub opaque type Message {
  // --- Configuration (always allowed) ---
  WelcomeMessage(reply: Subject(Option(String)))
  WithMode(mode: Mode, reply: Subject(Nil))
  WithNatWorkaround(enabled: Bool, reply: Subject(Nil))
  WithActiveMode(timeout: Int, reply: Subject(Nil))
  WithPassiveStreamBuilder(builder: PassiveStreamBuilder, reply: Subject(Nil))

  // --- Security (chunk guard, updates client) ---
  IntoSecure(ssl_options: kafein.WrapOptions, reply: Subject(FtpResult(Nil)))
  ClearCommandChannel(reply: Subject(FtpResult(Nil)))

  // --- Control commands (chunk guard) ---
  Login(username: String, password: String, reply: Subject(FtpResult(Nil)))
  Noop(reply: Subject(FtpResult(Nil)))
  Pwd(reply: Subject(FtpResult(String)))
  Cwd(path: String, reply: Subject(FtpResult(Nil)))
  Cdup(reply: Subject(FtpResult(Nil)))
  Mkd(path: String, reply: Subject(FtpResult(Nil)))
  Rmd(path: String, reply: Subject(FtpResult(Nil)))
  Dele(path: String, reply: Subject(FtpResult(Nil)))
  Rename(from: String, to: String, reply: Subject(FtpResult(Nil)))
  TransferType(file_type: file_type.FileType, reply: Subject(FtpResult(Nil)))
  Rest(offset: Int, reply: Subject(FtpResult(Nil)))
  Abor(reply: Subject(FtpResult(Nil)))
  Mdtm(pathname: String, reply: Subject(FtpResult(Timestamp)))
  Size(pathname: String, reply: Subject(FtpResult(Int)))
  Feat(reply: Subject(FtpResult(Features)))
  Opts(option: String, value: Option(String), reply: Subject(FtpResult(Nil)))
  SiteCmd(sub_command: String, reply: Subject(FtpResult(Response)))
  Eprt(
    address: String,
    port: Int,
    ip_version: command.IpVersion,
    reply: Subject(FtpResult(Nil)),
  )
  CustomCommand(
    command_str: String,
    expected_statuses: List(Status),
    reply: Subject(FtpResult(Response)),
  )
  Mlst(pathname: Option(String), reply: Subject(FtpResult(String)))

  // --- Callback-based data commands (chunk guard, blocks actor during execution) ---
  Retr(
    path: String,
    reader: fn(DataStream) -> FtpResult(Nil),
    reply: Subject(FtpResult(Nil)),
  )
  Stor(
    path: String,
    writer: fn(DataStream) -> FtpResult(Nil),
    reply: Subject(FtpResult(Nil)),
  )
  Appe(
    path: String,
    writer: fn(DataStream) -> FtpResult(Nil),
    reply: Subject(FtpResult(Nil)),
  )
  ListDir(pathname: Option(String), reply: Subject(FtpResult(List(String))))
  Nlst(pathname: Option(String), reply: Subject(FtpResult(List(String))))
  Mlsd(pathname: Option(String), reply: Subject(FtpResult(List(String))))
  CustomDataCommand(
    command_str: String,
    expected_statuses: List(Status),
    on_data_stream: fn(DataStream, Response) -> FtpResult(Nil),
    reply: Subject(FtpResult(Nil)),
  )

  // --- Open data channel (chunk guard, sets chunk=True) ---
  OpenRetr(path: String, reply: Subject(FtpResult(DataStream)))
  OpenStor(path: String, reply: Subject(FtpResult(DataStream)))
  OpenAppe(path: String, reply: Subject(FtpResult(DataStream)))
  OpenList(pathname: Option(String), reply: Subject(FtpResult(DataStream)))
  OpenNlst(pathname: Option(String), reply: Subject(FtpResult(DataStream)))
  OpenMlsd(pathname: Option(String), reply: Subject(FtpResult(DataStream)))
  OpenDataCommand(
    command_str: String,
    expected_statuses: List(Status),
    reply: Subject(FtpResult(#(DataStream, Response))),
  )

  // --- Close data channel (resets chunk=False) ---
  CloseDataChannel(data_stream: DataStream, reply: Subject(FtpResult(Nil)))

  // --- Lifecycle ---
  Quit(reply: Subject(FtpResult(Nil)))
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Start an FTP actor wrapping the given client.
///
/// The client should already be connected (via `gftp.connect`).
/// All subsequent operations should go through the returned `Handle`.
/// 
/// You should never keep an active `FtpClient` around after starting the actor,
/// as it would allow bypassing the chunk protection and corrupting the protocol state.
/// Only interact with the FTP session via the returned `Handle` and the functions in this module.
pub fn start(
  client: FtpClient,
) -> Result(actor.Started(Handle), actor.StartError) {
  State(client: client, chunk: False)
  |> actor.new()
  |> actor.on_message(handle_message)
  |> actor.start()
}

// --- Configuration (always allowed) ---

/// Get the welcome message from the FTP server.
pub fn welcome_message(handle: Handle) -> Option(String) {
  actor.call(handle, default_call_timeout, WelcomeMessage)
}

/// Set the data transfer mode.
pub fn with_mode(handle: Handle, mode: Mode) -> Nil {
  actor.call(handle, default_call_timeout, WithMode(mode, _))
}

/// Enable or disable the NAT workaround for passive mode.
pub fn with_nat_workaround(handle: Handle, enabled: Bool) -> Nil {
  actor.call(handle, default_call_timeout, WithNatWorkaround(enabled, _))
}

/// Enable active mode with the specified data connection timeout.
pub fn with_active_mode(handle: Handle, timeout: Int) -> Nil {
  actor.call(handle, default_call_timeout, WithActiveMode(timeout, _))
}

/// Set a custom passive stream builder.
pub fn with_passive_stream_builder(
  handle: Handle,
  builder: PassiveStreamBuilder,
) -> Nil {
  actor.call(handle, default_call_timeout, WithPassiveStreamBuilder(builder, _))
}

// --- Security ---

/// Switch to explicit secure mode (FTPS).
pub fn into_secure(
  handle: Handle,
  ssl_options: kafein.WrapOptions,
) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, IntoSecure(ssl_options, _))
}

/// Clear the command channel encryption.
pub fn clear_command_channel(handle: Handle) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, ClearCommandChannel)
}

// --- Control commands ---

/// Log in to the FTP server.
pub fn login(
  handle: Handle,
  username: String,
  password: String,
) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Login(username, password, _))
}

/// Send a NOOP command.
pub fn noop(handle: Handle) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Noop)
}

/// Get the current working directory.
pub fn pwd(handle: Handle) -> FtpResult(String) {
  actor.call(handle, default_call_timeout, Pwd)
}

/// Change working directory.
pub fn cwd(handle: Handle, path: String) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Cwd(path, _))
}

/// Change to parent directory.
pub fn cdup(handle: Handle) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Cdup)
}

/// Create a directory.
pub fn mkd(handle: Handle, path: String) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Mkd(path, _))
}

/// Remove a directory.
pub fn rmd(handle: Handle, path: String) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Rmd(path, _))
}

/// Delete a file.
pub fn dele(handle: Handle, path: String) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Dele(path, _))
}

/// Rename a file.
pub fn rename(handle: Handle, from: String, to: String) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Rename(from, to, _))
}

/// Set the file transfer type.
pub fn transfer_type(
  handle: Handle,
  file_type: file_type.FileType,
) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, TransferType(file_type, _))
}

/// Set the restart offset for the next transfer.
pub fn rest(handle: Handle, offset: Int) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Rest(offset, _))
}

/// Abort an active file transfer.
pub fn abor(handle: Handle) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Abor)
}

/// Get the modification time of a file.
pub fn mdtm(handle: Handle, pathname: String) -> FtpResult(Timestamp) {
  actor.call(handle, default_call_timeout, Mdtm(pathname, _))
}

/// Get the size of a file in bytes.
pub fn size(handle: Handle, pathname: String) -> FtpResult(Int) {
  actor.call(handle, default_call_timeout, Size(pathname, _))
}

/// Retrieve server features (FEAT command).
pub fn feat(handle: Handle) -> FtpResult(Features) {
  actor.call(handle, default_call_timeout, Feat)
}

/// Set a server option (OPTS command).
pub fn opts(
  handle: Handle,
  option: String,
  value: Option(String),
) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Opts(option, value, _))
}

/// Execute a SITE command.
pub fn site(handle: Handle, sub_command: String) -> FtpResult(Response) {
  actor.call(handle, default_call_timeout, SiteCmd(sub_command, _))
}

/// Execute an EPRT command.
pub fn eprt(
  handle: Handle,
  address: String,
  port: Int,
  ip_version: command.IpVersion,
) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Eprt(address, port, ip_version, _))
}

/// Execute a custom FTP command.
pub fn custom_command(
  handle: Handle,
  command_str: String,
  expected_statuses: List(Status),
) -> FtpResult(Response) {
  actor.call(handle, default_call_timeout, CustomCommand(
    command_str,
    expected_statuses,
    _,
  ))
}

/// Execute an MLST command.
pub fn mlst(handle: Handle, pathname: Option(String)) -> FtpResult(String) {
  actor.call(handle, default_call_timeout, Mlst(pathname, _))
}

// --- Callback-based data commands ---

/// Download a file using a callback.
pub fn retr(
  handle: Handle,
  path: String,
  reader: fn(DataStream) -> FtpResult(Nil),
) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Retr(path, reader, _))
}

/// Upload a file using a callback.
pub fn stor(
  handle: Handle,
  path: String,
  writer: fn(DataStream) -> FtpResult(Nil),
) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Stor(path, writer, _))
}

/// Append to a file using a callback.
pub fn appe(
  handle: Handle,
  path: String,
  writer: fn(DataStream) -> FtpResult(Nil),
) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Appe(path, writer, _))
}

/// List directory contents.
pub fn list(handle: Handle, pathname: Option(String)) -> FtpResult(List(String)) {
  actor.call(handle, default_call_timeout, ListDir(pathname, _))
}

/// List file names only.
pub fn nlst(handle: Handle, pathname: Option(String)) -> FtpResult(List(String)) {
  actor.call(handle, default_call_timeout, Nlst(pathname, _))
}

/// Machine-readable directory listing.
pub fn mlsd(handle: Handle, pathname: Option(String)) -> FtpResult(List(String)) {
  actor.call(handle, default_call_timeout, Mlsd(pathname, _))
}

/// Execute a custom data command with a callback.
pub fn custom_data_command(
  handle: Handle,
  command_str: String,
  expected_statuses: List(Status),
  on_data_stream: fn(DataStream, Response) -> FtpResult(Nil),
) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, CustomDataCommand(
    command_str,
    expected_statuses,
    on_data_stream,
    _,
  ))
}

// --- Message-based streaming ---

/// Open a data channel for downloading a file.
pub fn open_retr(handle: Handle, path: String) -> FtpResult(DataStream) {
  actor.call(handle, default_call_timeout, OpenRetr(path, _))
}

/// Open a data channel for uploading a file.
pub fn open_stor(handle: Handle, path: String) -> FtpResult(DataStream) {
  actor.call(handle, default_call_timeout, OpenStor(path, _))
}

/// Open a data channel for appending to a file.
pub fn open_appe(handle: Handle, path: String) -> FtpResult(DataStream) {
  actor.call(handle, default_call_timeout, OpenAppe(path, _))
}

/// Open a data channel for a LIST directory listing.
pub fn open_list(
  handle: Handle,
  pathname: Option(String),
) -> FtpResult(DataStream) {
  actor.call(handle, default_call_timeout, OpenList(pathname, _))
}

/// Open a data channel for an NLST file name listing.
pub fn open_nlst(
  handle: Handle,
  pathname: Option(String),
) -> FtpResult(DataStream) {
  actor.call(handle, default_call_timeout, OpenNlst(pathname, _))
}

/// Open a data channel for an MLSD machine-readable listing.
pub fn open_mlsd(
  handle: Handle,
  pathname: Option(String),
) -> FtpResult(DataStream) {
  actor.call(handle, default_call_timeout, OpenMlsd(pathname, _))
}

/// Open a data channel for a custom command.
pub fn open_data_command(
  handle: Handle,
  command_str: String,
  expected_statuses: List(Status),
) -> FtpResult(#(DataStream, Response)) {
  actor.call(handle, default_call_timeout, OpenDataCommand(
    command_str,
    expected_statuses,
    _,
  ))
}

/// Close a data channel and finalize the transfer.
pub fn close_data_channel(
  handle: Handle,
  data_stream: DataStream,
) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, CloseDataChannel(data_stream, _))
}

// --- Lifecycle ---

/// Quit the FTP session and stop the actor.
pub fn quit(handle: Handle) -> FtpResult(Nil) {
  actor.call(handle, default_call_timeout, Quit)
}

// ---------------------------------------------------------------------------
// Message handler
// ---------------------------------------------------------------------------

fn handle_message(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    // --- Configuration (always allowed) ---
    WelcomeMessage(reply) -> {
      process.send(reply, gftp.welcome_message(state.client))
      actor.continue(state)
    }
    WithMode(mode, reply) -> {
      let client = gftp.with_mode(state.client, mode)
      process.send(reply, Nil)
      actor.continue(State(..state, client: client))
    }
    WithNatWorkaround(enabled, reply) -> {
      let client = gftp.with_nat_workaround(state.client, enabled)
      process.send(reply, Nil)
      actor.continue(State(..state, client: client))
    }
    WithActiveMode(timeout, reply) -> {
      let client = gftp.with_active_mode(state.client, timeout)
      process.send(reply, Nil)
      actor.continue(State(..state, client: client))
    }
    WithPassiveStreamBuilder(builder, reply) -> {
      let client = gftp.with_passive_stream_builder(state.client, builder)
      process.send(reply, Nil)
      actor.continue(State(..state, client: client))
    }

    // --- Security (chunk guard, updates client) ---
    IntoSecure(ssl_options, reply) ->
      guard_chunk(state, reply, fn(s) {
        case gftp.into_secure(s.client, ssl_options) {
          Ok(new_client) -> #(State(..s, client: new_client), Ok(Nil))
          Error(e) -> #(s, Error(e))
        }
      })
    ClearCommandChannel(reply) ->
      guard_chunk(state, reply, fn(s) {
        case gftp.clear_command_channel(s.client) {
          Ok(new_client) -> #(State(..s, client: new_client), Ok(Nil))
          Error(e) -> #(s, Error(e))
        }
      })

    // --- Control commands (chunk guard) ---
    Login(username, password, reply) ->
      guard_chunk(state, reply, fn(s) {
        #(s, gftp.login(s.client, username, password))
      })
    Noop(reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.noop(s.client)) })
    Pwd(reply) -> guard_chunk(state, reply, fn(s) { #(s, gftp.pwd(s.client)) })
    Cwd(path, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.cwd(s.client, path)) })
    Cdup(reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.cdup(s.client)) })
    Mkd(path, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.mkd(s.client, path)) })
    Rmd(path, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.rmd(s.client, path)) })
    Dele(path, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.dele(s.client, path)) })
    Rename(from, to, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.rename(s.client, from, to)) })
    TransferType(file_type, reply) ->
      guard_chunk(state, reply, fn(s) {
        #(s, gftp.transfer_type(s.client, file_type))
      })
    Rest(offset, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.rest(s.client, offset)) })
    Abor(reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.abor(s.client)) })
    Mdtm(pathname, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.mdtm(s.client, pathname)) })
    Size(pathname, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.size(s.client, pathname)) })
    Feat(reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.feat(s.client)) })
    Opts(option, value, reply) ->
      guard_chunk(state, reply, fn(s) {
        #(s, gftp.opts(s.client, option, value))
      })
    SiteCmd(sub_command, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.site(s.client, sub_command)) })
    Eprt(address, port, ip_version, reply) ->
      guard_chunk(state, reply, fn(s) {
        #(s, gftp.eprt(s.client, address, port, ip_version))
      })
    CustomCommand(command_str, expected_statuses, reply) ->
      guard_chunk(state, reply, fn(s) {
        #(s, gftp.custom_command(s.client, command_str, expected_statuses))
      })
    Mlst(pathname, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.mlst(s.client, pathname)) })

    // --- Callback-based data commands (chunk guard) ---
    Retr(path, reader, reply) ->
      guard_chunk(state, reply, fn(s) {
        #(s, gftp.retr(s.client, path, reader))
      })
    Stor(path, writer, reply) ->
      guard_chunk(state, reply, fn(s) {
        #(s, gftp.stor(s.client, path, writer))
      })
    Appe(path, writer, reply) ->
      guard_chunk(state, reply, fn(s) {
        #(s, gftp.appe(s.client, path, writer))
      })
    ListDir(pathname, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.list(s.client, pathname)) })
    Nlst(pathname, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.nlst(s.client, pathname)) })
    Mlsd(pathname, reply) ->
      guard_chunk(state, reply, fn(s) { #(s, gftp.mlsd(s.client, pathname)) })
    CustomDataCommand(command_str, expected_statuses, on_data_stream, reply) ->
      guard_chunk(state, reply, fn(s) {
        #(
          s,
          gftp.custom_data_command(
            s.client,
            command_str,
            expected_statuses,
            on_data_stream,
          ),
        )
      })

    // --- Open data channel (chunk guard, sets chunk=True) ---
    OpenRetr(path, reply) ->
      guard_chunk_open(state, reply, fn(s) {
        data_channel.open_retr(s.client, path)
      })
    OpenStor(path, reply) ->
      guard_chunk_open(state, reply, fn(s) {
        data_channel.open_stor(s.client, path)
      })
    OpenAppe(path, reply) ->
      guard_chunk_open(state, reply, fn(s) {
        data_channel.open_appe(s.client, path)
      })
    OpenList(pathname, reply) ->
      guard_chunk_open(state, reply, fn(s) {
        data_channel.open_list(s.client, pathname)
      })
    OpenNlst(pathname, reply) ->
      guard_chunk_open(state, reply, fn(s) {
        data_channel.open_nlst(s.client, pathname)
      })
    OpenMlsd(pathname, reply) ->
      guard_chunk_open(state, reply, fn(s) {
        data_channel.open_mlsd(s.client, pathname)
      })
    OpenDataCommand(command_str, expected_statuses, reply) ->
      guard_chunk_open_pair(state, reply, fn(s) {
        data_channel.open_data_command(s.client, command_str, expected_statuses)
      })

    // --- Close data channel ---
    CloseDataChannel(data_stream, reply) -> {
      let result = data_channel.close_data_channel(state.client, data_stream)
      process.send(reply, result)
      actor.continue(State(..state, chunk: False))
    }

    // --- Lifecycle ---
    Quit(reply) -> {
      let result = gftp.quit(state.client)
      let _ = gftp.shutdown(state.client)
      process.send(reply, result)
      actor.stop()
    }
  }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Guard against issuing control commands while a data channel is open.
/// If `chunk` is `True`, replies with `DataTransferInProgress` and continues.
/// Otherwise, runs the operation and continues with the (possibly updated) state.
fn guard_chunk(
  state: State,
  reply: Subject(FtpResult(a)),
  operation: fn(State) -> #(State, FtpResult(a)),
) -> actor.Next(State, Message) {
  case state.chunk {
    True -> {
      process.send(reply, Error(ftp_result.DataTransferInProgress))
      actor.continue(state)
    }
    False -> {
      let #(new_state, result) = operation(state)
      process.send(reply, result)
      actor.continue(new_state)
    }
  }
}

/// Guard for open_* commands that return a `DataStream`.
/// On success, sets `chunk = True`.
fn guard_chunk_open(
  state: State,
  reply: Subject(FtpResult(DataStream)),
  operation: fn(State) -> FtpResult(DataStream),
) -> actor.Next(State, Message) {
  case state.chunk {
    True -> {
      process.send(reply, Error(ftp_result.DataTransferInProgress))
      actor.continue(state)
    }
    False -> {
      let result = operation(state)
      let new_chunk = case result {
        Ok(_) -> True
        Error(_) -> False
      }
      process.send(reply, result)
      actor.continue(State(..state, chunk: new_chunk))
    }
  }
}

/// Guard for open_data_command which returns a `#(DataStream, Response)`.
/// On success, sets `chunk = True`.
fn guard_chunk_open_pair(
  state: State,
  reply: Subject(FtpResult(#(DataStream, Response))),
  operation: fn(State) -> FtpResult(#(DataStream, Response)),
) -> actor.Next(State, Message) {
  case state.chunk {
    True -> {
      process.send(reply, Error(ftp_result.DataTransferInProgress))
      actor.continue(state)
    }
    False -> {
      let result = operation(state)
      let new_chunk = case result {
        Ok(_) -> True
        Error(_) -> False
      }
      process.send(reply, result)
      actor.continue(State(..state, chunk: new_chunk))
    }
  }
}
