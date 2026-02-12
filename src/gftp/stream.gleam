//// The stream module provides an interface for working with a network stream, which can either be a TCP stream or a TLS stream.
//// It abstracts away the differences between the two types of streams and provides a unified interface for reading and writing data.
////
//// ## Message-based mode
////
//// For integration with OTP actors and other message-based architectures, the stream module provides
//// a non-blocking alternative to synchronous `receive`. Instead of blocking the calling process,
//// you can use `receive_next_packet_as_message` to arm the socket for a single message delivery,
//// then use `select_stream_messages` to build a `process.Selector` that maps incoming socket
//// messages to your actor's message type.
////
//// This is the standard BEAM approach: set `{active, once}` on the socket, receive one message,
//// then re-arm. This gives backpressure control and integrates naturally with `gleam_erlang`
//// selectors and OTP actors.

import gftp/result as ftp_result
import gleam/erlang/process
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

/// A unified message type for data received asynchronously from a `DataStream`.
///
/// Used with `receive_next_packet_as_message` and `select_stream_messages` for
/// non-blocking, message-based I/O that integrates with OTP actors.
pub type StreamMessage {
  /// Data received from the stream.
  Packet(BitArray)
  /// The remote peer closed the connection.
  StreamClosed
  /// An error occurred on the stream.
  StreamError(ftp_result.FtpError)
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

/// Set `{active, once}` on the underlying socket so that the next packet is
/// delivered as an Erlang message to the calling process instead of being
/// buffered for a synchronous `receive` call.
///
/// After each message arrives you must call this again to receive the next one.
/// Use `select_stream_messages` to build a selector that maps the raw socket
/// messages into `StreamMessage` values.
pub fn receive_next_packet_as_message(stream: DataStream) -> Nil {
  case stream {
    Ssl(ssl_socket, _) -> kafein.receive_next_packet_as_message(ssl_socket)
    Tcp(tcp_socket) -> mug.receive_next_packet_as_message(tcp_socket)
  }
}

/// Add handlers for both TCP and SSL socket messages to a `process.Selector`.
///
/// The `mapper` function converts a `StreamMessage` into your actor's message
/// type. Handlers for both TCP and SSL messages are registered; only the one
/// matching the actual socket type will ever fire.
///
/// ```gleam
/// import gleam/erlang/process
/// import gftp/stream.{type StreamMessage}
///
/// type MyMessage {
///   GotStream(StreamMessage)
///   // ... other variants
/// }
///
/// let selector =
///   process.new_selector()
///   |> stream.select_stream_messages(GotStream)
/// ```
pub fn select_stream_messages(
  selector: process.Selector(t),
  mapper: fn(StreamMessage) -> t,
) -> process.Selector(t) {
  selector
  |> mug.select_tcp_messages(fn(tcp_msg) {
    mapper(case tcp_msg {
      mug.Packet(_, data) -> Packet(data)
      mug.SocketClosed(_) -> StreamClosed
      mug.TcpError(_, err) -> StreamError(ftp_result.Socket(err))
    })
  })
  |> kafein.select_ssl_messages(fn(ssl_msg) {
    mapper(case ssl_msg {
      kafein.Packet(_, data) -> Packet(data)
      kafein.SocketClosed(_) -> StreamClosed
      kafein.SslError(_, err) -> StreamError(ftp_result.Tls(err))
    })
  })
}

/// Shuts down the provided stream, whether it's an SSL or TCP stream.
/// It abstracts away the differences between the two types of streams and provides a unified interface for shutting them down.
pub fn shutdown(stream: DataStream) -> Result(Nil, mug.Error) {
  case stream {
    Ssl(ssl_socket, _) -> kafein.shutdown(ssl_socket)
    Tcp(tcp_socket) -> mug.shutdown(tcp_socket)
  }
}
