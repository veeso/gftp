import gftp/stream.{Tcp}
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

  let assert Ok(<<"hello world":utf8>>) =
    stream.receive(Tcp(client_socket), 5000)

  close_server(server_socket)
  close_listen(listen_socket)
  let _ = stream.shutdown(Tcp(client_socket))
  Nil
}

pub fn receive_exact_tcp_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)
  let server_socket = accept(listen_socket)

  server_send(server_socket, <<"hello world":utf8>>)

  let assert Ok(<<"h":utf8>>) =
    stream.receive_exact(Tcp(client_socket), 1, 5000)

  close_server(server_socket)
  close_listen(listen_socket)
  let _ = stream.shutdown(Tcp(client_socket))
  Nil
}

pub fn send_tcp_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)
  let server_socket = accept(listen_socket)

  let assert Ok(_) = stream.send(Tcp(client_socket), <<"hello from client":utf8>>)

  let assert Ok(<<"hello from client":utf8>>) = server_recv(server_socket, 5000)

  close_server(server_socket)
  close_listen(listen_socket)
  let _ = stream.shutdown(Tcp(client_socket))
  Nil
}

pub fn shutdown_tcp_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)
  let server_socket = accept(listen_socket)

  let assert Ok(_) = stream.shutdown(Tcp(client_socket))

  close_server(server_socket)
  close_listen(listen_socket)
}

pub fn receive_tcp_timeout_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)
  let server_socket = accept(listen_socket)

  // No data sent, so receive should timeout
  let assert Error(_) = stream.receive(Tcp(client_socket), 100)

  close_server(server_socket)
  close_listen(listen_socket)
  let _ = stream.shutdown(Tcp(client_socket))
  Nil
}

pub fn receive_exact_tcp_timeout_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)
  let server_socket = accept(listen_socket)

  // No data sent, so receive_exact should timeout
  let assert Error(_) = stream.receive_exact(Tcp(client_socket), 1, 100)

  close_server(server_socket)
  close_listen(listen_socket)
  let _ = stream.shutdown(Tcp(client_socket))
  Nil
}

pub fn peer_address_tcp_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)
  let _server_socket = accept(listen_socket)

  let assert Ok(#(ip, peer_port)) = stream.peer_address(Tcp(client_socket))
  let assert "127.0.0.1" = ip
  let assert True = peer_port == port

  close_listen(listen_socket)
  let _ = stream.shutdown(Tcp(client_socket))
  Nil
}

pub fn local_address_tcp_test() {
  let listen_socket = listen()
  let port = listen_port(listen_socket)
  let client_socket = connect_client(port)

  let assert Ok(#(ip, local_port)) = stream.local_address(Tcp(client_socket))
  let assert "127.0.0.1" = ip
  let assert True = local_port > 0

  close_listen(listen_socket)
  let _ = stream.shutdown(Tcp(client_socket))
  Nil
}
