import gftp/listener
import gleeunit/should
import mug

pub fn listen_and_port_test() {
  let assert Ok(listen_socket) = listener.listen()
  let assert Ok(port) = listener.port(listen_socket)
  should.be_true(port > 0)
  listener.close(listen_socket)
}

pub fn accept_test() {
  let assert Ok(listen_socket) = listener.listen()
  let assert Ok(port) = listener.port(listen_socket)

  // Connect a client to the listener
  let assert Ok(_client_socket) =
    mug.new("127.0.0.1", port: port)
    |> mug.timeout(milliseconds: 5000)
    |> mug.connect()

  // Accept the connection
  let assert Ok(_accepted_socket) = listener.accept(listen_socket, 5000)

  listener.close(listen_socket)
}

pub fn accept_timeout_test() {
  let assert Ok(listen_socket) = listener.listen()

  // No client connects, so accept should timeout
  listener.accept(listen_socket, 100)
  |> should.be_error()

  listener.close(listen_socket)
}
