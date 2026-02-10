import gftp/stream.{Tcp}
import gleeunit/should
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

fn connect_client(port: Int) -> mug.Socket {
  let assert Ok(socket) =
    mug.new("127.0.0.1", port: port)
    |> mug.timeout(milliseconds: 5000)
    |> mug.ip_version_preference(mug.Ipv4Only)
    |> mug.connect()
  socket
}

pub fn receive_tcp_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)
  let server_socket = accept(listen_socket)

  server_send(server_socket, <<"hello world":utf8>>)

  stream.receive(Tcp(client_socket, "127.0.0.1", port), 5000)
  |> should.be_ok()
  |> should.equal(<<"hello world":utf8>>)

  close_server(server_socket)
  close_listen(listen_socket)
  let _ = stream.shutdown(Tcp(client_socket, "127.0.0.1", port))
  Nil
}

pub fn receive_exact_tcp_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)
  let server_socket = accept(listen_socket)

  server_send(server_socket, <<"hello world":utf8>>)

  stream.receive_exact(Tcp(client_socket, "127.0.0.1", port), 1, 5000)
  |> should.be_ok()
  |> should.equal(<<"h":utf8>>)

  close_server(server_socket)
  close_listen(listen_socket)
  let _ = stream.shutdown(Tcp(client_socket, "127.0.0.1", port))
  Nil
}

pub fn send_tcp_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)
  let server_socket = accept(listen_socket)

  stream.send(Tcp(client_socket, "127.0.0.1", port), <<
    "hello from client":utf8,
  >>)
  |> should.be_ok()

  server_recv(server_socket, 5000)
  |> should.be_ok()
  |> should.equal(<<"hello from client":utf8>>)

  close_server(server_socket)
  close_listen(listen_socket)
  let _ = stream.shutdown(Tcp(client_socket, "127.0.0.1", port))
  Nil
}

pub fn shutdown_tcp_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)
  let server_socket = accept(listen_socket)

  stream.shutdown(Tcp(client_socket, "127.0.0.1", port))
  |> should.be_ok()

  close_server(server_socket)
  close_listen(listen_socket)
}

pub fn receive_tcp_timeout_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)
  let server_socket = accept(listen_socket)

  // No data sent, so receive should timeout
  stream.receive(Tcp(client_socket, "127.0.0.1", port), 100)
  |> should.be_error()

  close_server(server_socket)
  close_listen(listen_socket)
  let _ = stream.shutdown(Tcp(client_socket, "127.0.0.1", port))
  Nil
}

pub fn receive_exact_tcp_timeout_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)
  let server_socket = accept(listen_socket)

  // No data sent, so receive_exact should timeout
  stream.receive_exact(Tcp(client_socket, "127.0.0.1", port), 1, 100)
  |> should.be_error()

  close_server(server_socket)
  close_listen(listen_socket)
  let _ = stream.shutdown(Tcp(client_socket, "127.0.0.1", port))
  Nil
}

pub fn socket_address_tcp_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)

  let address = stream.socket_address(Tcp(client_socket, "127.0.0.1", port))
  assert address == #("127.0.0.1", port)
}
