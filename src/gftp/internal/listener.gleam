//// The listener module provides a TCP listener for active mode FTP data connections.
//// It wraps Erlang's gen_tcp:listen/accept to allow the client to accept incoming
//// connections from the FTP server.

import mug

/// A type representing a TCP listen socket.
pub type ListenSocket {
  ListenSocket
}

/// IP address family for the listener socket.
pub type IpFamily {
  Ipv4
  Ipv6
}

/// Create a TCP listener bound to an ephemeral port on all interfaces.
/// The `ip_family` parameter determines whether to bind an IPv4 or IPv6 socket.
/// Returns the listen socket on success.
@external(erlang, "listener_ffi", "listen")
pub fn listen(ip_family: IpFamily) -> Result(ListenSocket, mug.Error)

/// Get the port number assigned to the listen socket.
@external(erlang, "listener_ffi", "listener_port")
pub fn port(socket: ListenSocket) -> Result(Int, mug.Error)

/// Accept an incoming connection on the listen socket with a timeout in milliseconds.
/// Returns the accepted socket (compatible with mug.Socket).
@external(erlang, "listener_ffi", "accept")
pub fn accept(
  socket: ListenSocket,
  timeout: Int,
) -> Result(mug.Socket, mug.Error)

/// Close the listen socket.
@external(erlang, "listener_ffi", "close")
pub fn close(socket: ListenSocket) -> Nil
