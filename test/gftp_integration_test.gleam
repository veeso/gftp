import gftp.{type FtpClient}

@external(erlang, "stream_test_ffi", "get_env")
fn get_env(name: String) -> Result(String, Nil)

@external(erlang, "test_container_ffi", "start_ftp_container")
fn start_ftp_container() -> String

@external(erlang, "test_container_ffi", "get_ftp_port")
fn get_ftp_port(container_id: String) -> Int

@external(erlang, "test_container_ffi", "stop_container")
fn stop_container(container_id: String) -> Nil

fn require_integration() -> Bool {
  case get_env("GFTP_INTEGRATION_TESTS") {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Run an integration test with a real FTP connection.
/// Starts a Docker FTP container, connects, logs in,
/// runs the callback, then cleans up.
/// Skipped unless GFTP_INTEGRATION_TESTS env var is set.
fn with_ftp_connection(callback: fn(FtpClient) -> Nil) -> Nil {
  case require_integration() {
    False -> {
      Nil
    }
    True -> {
      let container_id = start_ftp_container()
      let port = get_ftp_port(container_id)
      //io.println("Connecting to FTP on port " <> int.to_string(port))
      let assert Ok(client) =
        gftp.connect_timeout("127.0.0.1", port, timeout: 30_000)
      let assert Ok(_) = gftp.login(client, "test", "test")
      callback(client)
      let assert Ok(_) = gftp.quit(client)
      stop_container(container_id)
    }
  }
}

pub fn connect_and_login_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_pwd) = gftp.pwd(client)
    Nil
  })
}
