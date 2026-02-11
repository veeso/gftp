//// The stream module provides an interface for working with a network stream, which can either be a TCP stream or a TLS stream.
//// It abstracts away the differences between the two types of streams and provides a unified interface for reading and writing data.

import kafein.{type WrapOptions}
import mug

/// Workaround for kafein bug: patches the WrapOptions to convert the
/// server_name_indication from a binary to a charlist before kafein.wrap
/// processes it. Remove once kafein fixes this upstream.
///
/// See <https://github.com/fuzzko/kafein/issues/1>
@external(erlang, "stream_ffi", "patch_wrap_options")
fn patch_wrap_options(options: WrapOptions) -> WrapOptions

@external(erlang, "stream_ffi", "set_tcp_packet_line")
fn set_tcp_packet_line(socket: mug.Socket) -> Nil

@external(erlang, "stream_ffi", "set_tcp_packet_raw")
fn set_tcp_packet_raw(socket: mug.Socket) -> Nil

@external(erlang, "stream_ffi", "set_ssl_packet_line")
fn set_ssl_packet_line(socket: kafein.SslSocket) -> Nil

@external(erlang, "stream_ffi", "set_ssl_packet_raw")
fn set_ssl_packet_raw(socket: kafein.SslSocket) -> Nil

@external(erlang, "stream_ffi", "local_address")
fn tcp_local_address(socket: mug.Socket) -> Result(#(String, Int), mug.Error)

@external(erlang, "stream_ffi", "peer_address")
fn tcp_peer_address(socket: mug.Socket) -> Result(#(String, Int), mug.Error)

/// Data Stream used for communications. It can be both of type Tcp in case of plain communication or Ssl in case of FTPS
pub type DataStream {
  /// Contains the SSL socket and the underlying TCP socket.
  /// The TCP socket is necessary in case we want to switch back to a plain TCP stream.
  Ssl(ssl: kafein.SslSocket, tcp: mug.Socket)
  Tcp(socket: mug.Socket)
}

/// Receive a message from the peer.
///
/// Errors if the socket is closed, if the timeout is reached, or if any other error occurs during the receive operation.
/// The error is returned as a [`mug.Error`]. In case of success, it returns the received data as a `BitArray`.
pub fn receive(stream: DataStream, timeout: Int) -> Result(BitArray, mug.Error) {
  case stream {
    Ssl(ssl_socket, _) -> kafein.receive(ssl_socket, timeout)
    Tcp(tcp_socket) -> mug.receive(tcp_socket, timeout)
  }
}

/// Receive an exact number of bytes from the peer.
///
/// Errors if the socket is closed, if the timeout is reached, or if any other error occurs during the receive operation.
/// The error is returned as a [`mug.Error`]. In case of success, it returns the received data as a `BitArray`.
pub fn receive_exact(
  stream,
  size: Int,
  timeout: Int,
) -> Result(BitArray, mug.Error) {
  case stream {
    Ssl(ssl_socket, _) -> kafein.receive_exact(ssl_socket, size, timeout)
    Tcp(tcp_socket) -> mug.receive_exact(tcp_socket, size, timeout)
  }
}

/// Sends data over the provided stream.
/// It handles both SSL and TCP streams, abstracting away the differences between them.
///
/// Returns a Result indicating success (`Nil`) or an error ([`mug.Error`]) if the send operation fails.
pub fn send(stream: DataStream, data: BitArray) -> Result(Nil, mug.Error) {
  case stream {
    Ssl(ssl_socket, _) -> kafein.send(ssl_socket, data)
    Tcp(tcp_socket) -> mug.send(tcp_socket, data)
  }
}

/// Upgrades a TCP stream to an SSL stream using the provided options. If the stream is already an SSL stream, it returns it as is.
pub fn upgrade_to_ssl(
  stream: DataStream,
  options: WrapOptions,
) -> Result(DataStream, kafein.Error) {
  case stream {
    Ssl(_, _) -> Ok(stream)
    // Already an SSL stream, return as is
    Tcp(tcp_socket) ->
      case kafein.wrap(patch_wrap_options(options), tcp_socket) {
        Ok(ssl_socket) -> Ok(Ssl(ssl: ssl_socket, tcp: tcp_socket))
        Error(e) -> Error(e)
      }
  }
}

/// Downgrades an SSL stream to a TCP stream. If the stream is already a TCP stream, it returns it as is.
pub fn downgrade_to_tcp(stream: DataStream) -> DataStream {
  case stream {
    Ssl(_, tcp_socket) -> Tcp(tcp_socket)
    Tcp(_) -> stream
  }
}

/// Retrieves the local address (IP and port) of the underlying TCP socket.
pub fn local_address(stream: DataStream) -> Result(#(String, Int), mug.Error) {
  case stream {
    Ssl(_, tcp_socket) -> tcp_local_address(tcp_socket)
    Tcp(tcp_socket) -> tcp_local_address(tcp_socket)
  }
}

/// Retrieves the peer address (IP and port) of the underlying TCP socket.
pub fn peer_address(stream: DataStream) -> Result(#(String, Int), mug.Error) {
  case stream {
    Ssl(_, tcp_socket) -> tcp_peer_address(tcp_socket)
    Tcp(tcp_socket) -> tcp_peer_address(tcp_socket)
  }
}

/// Set the socket to line-delimited packet mode ({packet, line}).
/// In this mode, recv returns one complete line per call.
pub fn set_line_mode(stream: DataStream) -> Nil {
  case stream {
    Ssl(ssl_socket, _) -> set_ssl_packet_line(ssl_socket)
    Tcp(tcp_socket) -> set_tcp_packet_line(tcp_socket)
  }
}

/// Set the socket back to raw packet mode ({packet, raw}).
/// In this mode, recv returns whatever data is available.
pub fn set_raw_mode(stream: DataStream) -> Nil {
  case stream {
    Ssl(ssl_socket, _) -> set_ssl_packet_raw(ssl_socket)
    Tcp(tcp_socket) -> set_tcp_packet_raw(tcp_socket)
  }
}

/// Shuts down the provided stream, whether it's an SSL or TCP stream.
/// It abstracts away the differences between the two types of streams and provides a unified interface for shutting them down.
pub fn shutdown(stream: DataStream) -> Result(Nil, mug.Error) {
  case stream {
    Ssl(ssl_socket, _) -> kafein.shutdown(ssl_socket)
    Tcp(tcp_socket) -> mug.shutdown(tcp_socket)
  }
}
