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

/// Data Stream used for communications. It can be both of type Tcp in case of plain communication or Ssl in case of FTPS
pub type DataStream {
  Ssl(kafein.SslSocket)
  Tcp(mug.Socket)
}

/// Receive a message from the peer.
/// 
/// Errors if the socket is closed, if the timeout is reached, or if any other error occurs during the receive operation.
/// The error is returned as a [`mug.Error`]. In case of success, it returns the received data as a `BitArray`.
pub fn receive(stream: DataStream, timeout: Int) -> Result(BitArray, mug.Error) {
  case stream {
    Ssl(ssl_socket) -> kafein.receive(ssl_socket, timeout)
    Tcp(tcp_socket) -> mug.receive(tcp_socket, timeout)
  }
}

/// Sends data over the provided stream.
/// It handles both SSL and TCP streams, abstracting away the differences between them.
/// 
/// Returns a Result indicating success (`Nil`) or an error ([`mug.Error`]) if the send operation fails.
pub fn send(stream: DataStream, data: BitArray) -> Result(Nil, mug.Error) {
  case stream {
    Ssl(ssl_socket) -> kafein.send(ssl_socket, data)
    Tcp(tcp_socket) -> mug.send(tcp_socket, data)
  }
}

/// Upgrades a TCP stream to an SSL stream using the provided options. If the stream is already an SSL stream, it returns it as is.
pub fn upgrade_to_ssl(
  stream: DataStream,
  options: WrapOptions,
) -> Result(DataStream, kafein.Error) {
  case stream {
    Ssl(_) -> Ok(stream)
    // Already an SSL stream, return as is
    Tcp(tcp_socket) ->
      case kafein.wrap(patch_wrap_options(options), tcp_socket) {
        Ok(ssl_socket) -> Ok(Ssl(ssl_socket))
        Error(e) -> Error(e)
      }
  }
}

/// Shuts down the provided stream, whether it's an SSL or TCP stream.
/// It abstracts away the differences between the two types of streams and provides a unified interface for shutting them down.
pub fn shutdown(stream: DataStream) -> Result(Nil, mug.Error) {
  case stream {
    Ssl(ssl_socket) -> kafein.shutdown(ssl_socket)
    Tcp(tcp_socket) -> mug.shutdown(tcp_socket)
  }
}
