import gftp
import gftp/actor as ftp_actor
import gftp/result as ftp_result
import gftp/stream
import gleam/bit_array
import gleam/erlang/process
import gleam/int
import gleam/option
import gleam/result
import mug

// Opaque types for server-side sockets (gen_tcp)
type ListenSocket

type ServerSocket

@external(erlang, "stream_test_ffi", "listen")
fn listen() -> ListenSocket

@external(erlang, "stream_test_ffi", "listen_port")
fn listen_port(socket: ListenSocket) -> Int

@external(erlang, "stream_test_ffi", "accept")
fn accept(socket: ListenSocket) -> ServerSocket

@external(erlang, "stream_test_ffi", "server_send")
fn server_send(socket: ServerSocket, data: BitArray) -> Nil

@external(erlang, "stream_test_ffi", "server_recv")
fn server_recv(socket: ServerSocket, timeout: Int) -> Result(BitArray, Nil)

@external(erlang, "stream_test_ffi", "close_listen")
fn close_listen(socket: ListenSocket) -> Nil

@external(erlang, "stream_test_ffi", "close_server")
fn close_server(socket: ServerSocket) -> Nil

/// Build a PASV response pointing to 127.0.0.1 and the given port
fn pasv_response(port: Int) -> BitArray {
  let p1 = port / 256
  let p2 = port % 256
  bit_array.from_string(
    "227 Entering Passive Mode (127,0,0,1,"
    <> int.to_string(p1)
    <> ","
    <> int.to_string(p2)
    <> ")\r\n",
  )
}

/// Connect a client to the fake server and create an actor.
/// The server sends a 220 welcome message and the actor reads it.
fn with_fake_ftp(
  callback: fn(ftp_actor.Handle, ServerSocket, ListenSocket) -> Nil,
) -> Nil {
  let listen_socket = listen()
  let port = listen_port(listen_socket)

  let assert Ok(client_socket) =
    mug.new("127.0.0.1", port)
    |> mug.timeout(milliseconds: 5000)
    |> mug.ip_version_preference(mug.Ipv4Only)
    |> mug.connect()

  let server_socket = accept(listen_socket)

  // Send FTP welcome
  server_send(server_socket, <<"220 Welcome\r\n":utf8>>)

  let assert Ok(client) = gftp.connect_with_stream(client_socket)
  let assert Ok(started) = ftp_actor.start(client)
  let handle = started.data

  callback(handle, server_socket, listen_socket)
}

/// Helper: respond to a PWD command from the client
fn respond_pwd(server_socket: ServerSocket) -> Nil {
  let assert Ok(_cmd) = server_recv(server_socket, 5000)
  server_send(server_socket, <<"257 \"/home/test\"\r\n":utf8>>)
}

/// Helper: respond to a QUIT command
fn respond_quit(server_socket: ServerSocket) -> Nil {
  let assert Ok(_cmd) = server_recv(server_socket, 5000)
  server_send(server_socket, <<"221 Goodbye\r\n":utf8>>)
}

/// Helper: open a data channel for STOR via fake server.
/// Returns the data stream and data server socket.
fn fake_open_stor(
  handle: ftp_actor.Handle,
  server_socket: ServerSocket,
) -> #(stream.DataStream, ServerSocket, ListenSocket) {
  let data_listen = listen()
  let data_port = listen_port(data_listen)

  let test_subject = process.new_subject()
  process.spawn(fn() {
    let result = ftp_actor.open_stor(handle, "test.txt")
    process.send(test_subject, result)
  })

  // Handle PASV
  let assert Ok(_) = server_recv(server_socket, 5000)
  server_send(server_socket, pasv_response(data_port))

  // Handle STOR command
  let assert Ok(_) = server_recv(server_socket, 5000)
  let data_server = accept(data_listen)
  server_send(server_socket, <<"150 Ok to send data\r\n":utf8>>)

  let assert Ok(Ok(data_stream)) = process.receive(test_subject, 5000)
  #(data_stream, data_server, data_listen)
}

/// Helper: close a data channel via fake server
fn fake_close_data_channel(
  handle: ftp_actor.Handle,
  data_stream: stream.DataStream,
  data_server: ServerSocket,
  server_socket: ServerSocket,
) -> Nil {
  let close_subject = process.new_subject()
  process.spawn(fn() {
    let result = ftp_actor.close_data_channel(handle, data_stream)
    process.send(close_subject, result)
  })
  close_server(data_server)
  server_send(server_socket, <<"226 Transfer complete\r\n":utf8>>)
  let assert Ok(Ok(_)) = process.receive(close_subject, 5000)
  Nil
}

/// Helper: quit actor via fake server
fn fake_quit(handle: ftp_actor.Handle, server_socket: ServerSocket) -> Nil {
  let quit_subject = process.new_subject()
  process.spawn(fn() {
    let result = ftp_actor.quit(handle)
    process.send(quit_subject, result)
  })
  respond_quit(server_socket)
  let assert Ok(Ok(_)) = process.receive(quit_subject, 5000)
  Nil
}

pub fn chunk_guard_rejects_control_during_open_data_channel_test() {
  with_fake_ftp(fn(handle, server_socket, listen_socket) {
    // Open a data channel
    let #(data_stream, data_server, data_listen) =
      fake_open_stor(handle, server_socket)

    // Control commands should be rejected
    let assert Error(ftp_result.DataTransferInProgress) = ftp_actor.pwd(handle)

    // Close the data channel
    fake_close_data_channel(handle, data_stream, data_server, server_socket)

    // Now control commands should work again
    let pwd_subject = process.new_subject()
    process.spawn(fn() { process.send(pwd_subject, ftp_actor.pwd(handle)) })
    respond_pwd(server_socket)
    let assert Ok(Ok("/home/test")) = process.receive(pwd_subject, 5000)

    // Cleanup
    fake_quit(handle, server_socket)
    close_listen(data_listen)
    close_server(server_socket)
    close_listen(listen_socket)
  })
}

pub fn config_allowed_during_open_data_channel_test() {
  with_fake_ftp(fn(handle, server_socket, listen_socket) {
    let #(data_stream, data_server, data_listen) =
      fake_open_stor(handle, server_socket)

    // Config commands should work during data transfer
    ftp_actor.with_nat_workaround(handle, True)
    let assert option.Some(_) = ftp_actor.welcome_message(handle)

    // Close and cleanup
    fake_close_data_channel(handle, data_stream, data_server, server_socket)
    fake_quit(handle, server_socket)
    close_listen(data_listen)
    close_server(server_socket)
    close_listen(listen_socket)
  })
}

pub fn callback_based_stor_test() {
  with_fake_ftp(fn(handle, server_socket, listen_socket) {
    let data_listen = listen()
    let data_port = listen_port(data_listen)

    let test_subject = process.new_subject()
    process.spawn(fn() {
      let result =
        ftp_actor.stor(handle, "test.txt", fn(ds) {
          stream.send(ds, <<"hello":utf8>>)
          |> result.map_error(ftp_result.Socket)
        })
      process.send(test_subject, result)
    })

    // Handle PASV
    let assert Ok(_) = server_recv(server_socket, 5000)
    server_send(server_socket, pasv_response(data_port))

    // Handle STOR
    let assert Ok(_) = server_recv(server_socket, 5000)
    let data_server = accept(data_listen)
    server_send(server_socket, <<"150 Ok to send data\r\n":utf8>>)

    // Read data sent by client
    let assert Ok(<<"hello":utf8>>) = server_recv(data_server, 5000)

    // Close data and send 226
    close_server(data_server)
    server_send(server_socket, <<"226 Transfer complete\r\n":utf8>>)

    let assert Ok(Ok(_)) = process.receive(test_subject, 5000)

    // After callback-based transfer, control commands should work
    let pwd_subject = process.new_subject()
    process.spawn(fn() { process.send(pwd_subject, ftp_actor.pwd(handle)) })
    respond_pwd(server_socket)
    let assert Ok(Ok("/home/test")) = process.receive(pwd_subject, 5000)

    // Cleanup
    fake_quit(handle, server_socket)
    close_listen(data_listen)
    close_server(server_socket)
    close_listen(listen_socket)
  })
}
