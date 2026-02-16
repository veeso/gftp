import envoy
import gftp/stream.{Ssl, Tcp}
import gleam/bit_array
import gleam/string
import kafein
import mug

fn require_integration() -> Bool {
  case envoy.get("GFTP_INTEGRATION_TESTS") {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn connect_tcp(host: String, port: Int) -> mug.Socket {
  let assert Ok(socket) =
    mug.new(host, port: port)
    |> mug.timeout(milliseconds: 10_000)
    |> mug.connect()
  socket
}

fn ssl_options(hostname: String) -> kafein.WrapOptions {
  kafein.default_options
  |> kafein.verify(verify_type: kafein.VerifyNone)
  |> kafein.server_name_indication(hostname: hostname)
}

pub fn integration_upgrade_to_ssl_test() {
  case require_integration() {
    False -> Nil
    True -> {
      let host = "example.com"
      let tcp_socket = connect_tcp(host, 443)
      let tcp_stream = Tcp(tcp_socket)

      // Upgrade TCP to SSL
      let assert Ok(ssl_stream) =
        stream.upgrade_to_ssl(tcp_stream, ssl_options(host))

      // Verify it's now an Ssl variant
      case ssl_stream {
        Ssl(_, _) -> Nil
        Tcp(_) -> panic as "expected Ssl variant after upgrade"
      }

      // Upgrading an already-SSL stream should return it unchanged
      let assert Ok(still_ssl) =
        stream.upgrade_to_ssl(ssl_stream, ssl_options(host))
      case still_ssl {
        Ssl(_, _) -> Nil
        Tcp(_) -> panic as "expected Ssl variant to remain after double upgrade"
      }

      let _ = stream.shutdown(ssl_stream)
      Nil
    }
  }
}

pub fn integration_downgrade_to_tcp_test() {
  case require_integration() {
    False -> Nil
    True -> {
      let host = "example.com"
      let tcp_socket = connect_tcp(host, 443)
      let tcp_stream = Tcp(tcp_socket)

      // Upgrade TCP to SSL
      let assert Ok(ssl_stream) =
        stream.upgrade_to_ssl(tcp_stream, ssl_options(host))

      // Verify it's now an Ssl variant
      case ssl_stream {
        Ssl(_, _) -> Nil
        Tcp(_) -> panic as "expected Ssl variant after upgrade"
      }

      // Upgrading an already-SSL stream should return it unchanged
      let tcp = stream.downgrade_to_tcp(ssl_stream)
      case tcp {
        Ssl(_, _) ->
          panic as "expected Ssl variant to remain after double upgrade"
        Tcp(_) -> Nil
      }

      // downgrading an already-TCP stream should return it unchanged
      let still_tcp = stream.downgrade_to_tcp(tcp)
      case still_tcp {
        Ssl(_, _) ->
          panic as "expected Ssl variant to remain after double upgrade"
        Tcp(_) -> Nil
      }

      let _ = stream.shutdown(ssl_stream)
      Nil
    }
  }
}

pub fn integration_send_receive_ssl_test() {
  case require_integration() {
    False -> Nil
    True -> {
      let host = "example.com"
      let tcp_socket = connect_tcp(host, 443)
      let tcp_stream = Tcp(tcp_socket)

      let assert Ok(ssl_stream) =
        stream.upgrade_to_ssl(tcp_stream, ssl_options(host))

      // Send an HTTP GET request
      let request =
        "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n"
      let assert Ok(_) = stream.send(ssl_stream, <<request:utf8>>)

      // Receive the response
      let assert Ok(response_bytes) = stream.receive(ssl_stream, 10_000)

      // Verify it looks like an HTTP response
      let assert Ok(response_text) = bit_array.to_string(response_bytes)
      let assert True = string.starts_with(response_text, "HTTP/1.1")

      let _ = stream.shutdown(ssl_stream)
      Nil
    }
  }
}

pub fn integration_shutdown_ssl_test() {
  case require_integration() {
    False -> Nil
    True -> {
      let host = "example.com"
      let tcp_socket = connect_tcp(host, 443)
      let tcp_stream = Tcp(tcp_socket)

      let assert Ok(ssl_stream) =
        stream.upgrade_to_ssl(tcp_stream, ssl_options(host))

      let assert Ok(_) = stream.shutdown(ssl_stream)
      Nil
    }
  }
}
