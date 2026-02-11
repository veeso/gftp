import gftp.{type FtpClient}
import gftp/command/file_type
import gftp/response
import gftp/result as ftp_result
import gftp/status
import gftp/stream
import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit/should
import kafein

@external(erlang, "stream_test_ffi", "get_env")
fn get_env(name: String) -> Result(String, Nil)

@external(erlang, "test_container_ffi", "start_ftp_container")
fn start_ftp_container() -> String

@external(erlang, "test_container_ffi", "get_ftp_port")
fn get_ftp_port(container_id: String) -> Int

@external(erlang, "test_container_ffi", "start_ftps_container")
fn start_ftps_container() -> String

@external(erlang, "test_container_ffi", "get_ftps_port")
fn get_ftps_port(container_id: String) -> Int

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
      let assert Ok(client) =
        gftp.connect_timeout("127.0.0.1", port, timeout: 30_000)
      let assert Ok(_) = gftp.login(client, "test", "test")
      callback(client)
      let assert Ok(_) = gftp.quit(client)
      stop_container(container_id)
    }
  }
}

// --- Connection and auth tests ---

pub fn connect_and_login_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_pwd) = gftp.pwd(client)
    Nil
  })
}

pub fn welcome_message_test() {
  with_ftp_connection(fn(client) {
    let msg = gftp.welcome_message(client)
    msg |> should.not_equal(None)
    Nil
  })
}

// --- Basic command tests ---

pub fn noop_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.noop(client)
    Nil
  })
}

pub fn pwd_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(pwd) = gftp.pwd(client)
    pwd |> should.equal("/home/test")
    Nil
  })
}

pub fn cwd_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(pwd) = gftp.pwd(client)
    let assert Ok(_) = gftp.cwd(client, "/")
    let assert Ok(new_pwd) = gftp.pwd(client)
    new_pwd |> should.equal("/")
    let assert Ok(_) = gftp.cwd(client, pwd)
    Nil
  })
}

pub fn cdup_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.cwd(client, "/home/test")
    let assert Ok(_) = gftp.cdup(client)
    let assert Ok(pwd) = gftp.pwd(client)
    pwd |> should.equal("/home")
    Nil
  })
}

// --- Directory management tests ---

pub fn mkd_and_rmd_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.mkd(client, "test_dir")
    let assert Ok(_) = gftp.cwd(client, "test_dir")
    let assert Ok(pwd) = gftp.pwd(client)
    pwd |> should.equal("/home/test/test_dir")
    let assert Ok(_) = gftp.cdup(client)
    let assert Ok(_) = gftp.rmd(client, "test_dir")
    Nil
  })
}

// --- Transfer type tests ---

pub fn transfer_type_binary_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    Nil
  })
}

pub fn transfer_type_ascii_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) =
      gftp.transfer_type(client, file_type.Ascii(file_type.Default))
    Nil
  })
}

// --- File transfer tests ---

pub fn stor_and_retr_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    let content = "Hello, gftp!"
    let assert Ok(_) =
      gftp.stor(client, "test_file.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string(content))
        |> result.map_error(ftp_result.Socket)
      })
    let assert Ok(_) =
      gftp.retr(client, "test_file.txt", fn(data_stream) {
        let assert Ok(data) = stream.receive(data_stream, 5000)
        let assert Ok(text) = bit_array.to_string(data)
        text |> should.equal(content)
        Ok(Nil)
      })
    let assert Ok(_) = gftp.dele(client, "test_file.txt")
    Nil
  })
}

pub fn appe_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    // Create initial file
    let assert Ok(_) =
      gftp.stor(client, "append_test.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string("Hello"))
        |> result.map_error(ftp_result.Socket)
      })
    // Append to it
    let assert Ok(_) =
      gftp.appe(client, "append_test.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string(", World!"))
        |> result.map_error(ftp_result.Socket)
      })
    // Verify combined content
    let assert Ok(_) =
      gftp.retr(client, "append_test.txt", fn(data_stream) {
        let assert Ok(data) = stream.receive(data_stream, 5000)
        let assert Ok(text) = bit_array.to_string(data)
        text |> should.equal("Hello, World!")
        Ok(Nil)
      })
    let assert Ok(_) = gftp.dele(client, "append_test.txt")
    Nil
  })
}

pub fn dele_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    // Create a file
    let assert Ok(_) =
      gftp.stor(client, "to_delete.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string("delete me"))
        |> result.map_error(ftp_result.Socket)
      })
    // Delete it
    let assert Ok(_) = gftp.dele(client, "to_delete.txt")
    // Verify it's gone by trying to get its size (should fail)
    gftp.size(client, "to_delete.txt") |> should.be_error
    Nil
  })
}

pub fn rename_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    // Create a file
    let assert Ok(_) =
      gftp.stor(client, "original.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string("rename me"))
        |> result.map_error(ftp_result.Socket)
      })
    // Rename it
    let assert Ok(_) = gftp.rename(client, "original.txt", "renamed.txt")
    // Verify old name is gone
    gftp.size(client, "original.txt") |> should.be_error
    // Verify new name exists and has correct content
    let assert Ok(_) =
      gftp.retr(client, "renamed.txt", fn(data_stream) {
        let assert Ok(data) = stream.receive(data_stream, 5000)
        let assert Ok(text) = bit_array.to_string(data)
        text |> should.equal("rename me")
        Ok(Nil)
      })
    let assert Ok(_) = gftp.dele(client, "renamed.txt")
    Nil
  })
}

pub fn size_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    let content = "1234567890"
    let assert Ok(_) =
      gftp.stor(client, "size_test.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string(content))
        |> result.map_error(ftp_result.Socket)
      })
    let assert Ok(file_size) = gftp.size(client, "size_test.txt")
    file_size |> should.equal(10)
    let assert Ok(_) = gftp.dele(client, "size_test.txt")
    Nil
  })
}

pub fn mdtm_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    let assert Ok(_) =
      gftp.stor(client, "mdtm_test.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string("test"))
        |> result.map_error(ftp_result.Socket)
      })
    // Should return a valid timestamp
    let assert Ok(_timestamp) = gftp.mdtm(client, "mdtm_test.txt")
    let assert Ok(_) = gftp.dele(client, "mdtm_test.txt")
    Nil
  })
}

// --- Directory listing tests ---

pub fn list_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    // Create a file so listing is non-empty
    let assert Ok(_) =
      gftp.stor(client, "list_test.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string("test"))
        |> result.map_error(ftp_result.Socket)
      })
    let assert Ok(lines) = gftp.list(client, None)
    // Should contain the file we created
    let has_file =
      lines |> list.any(fn(line) { string.contains(line, "list_test.txt") })
    has_file |> should.be_true
    let assert Ok(_) = gftp.dele(client, "list_test.txt")
    Nil
  })
}

pub fn list_with_path_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(lines) = gftp.list(client, Some("/home/test"))
    let _ = lines
    Nil
  })
}

pub fn nlst_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    let assert Ok(_) =
      gftp.stor(client, "nlst_test.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string("test"))
        |> result.map_error(ftp_result.Socket)
      })
    let assert Ok(names) = gftp.nlst(client, None)
    let has_file =
      names |> list.any(fn(name) { string.contains(name, "nlst_test.txt") })
    has_file |> should.be_true
    let assert Ok(_) = gftp.dele(client, "nlst_test.txt")
    Nil
  })
}

// mlsd_test disabled: vsftpd in test container doesn't support MLSD

// mlst_test disabled: vsftpd in test container doesn't support MLST

// --- FEAT command tests ---

pub fn feat_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(features) = gftp.feat(client)
    // vsftpd supports these features
    dict.has_key(features, "EPRT") |> should.be_true
    dict.has_key(features, "EPSV") |> should.be_true
    dict.has_key(features, "MDTM") |> should.be_true
    dict.has_key(features, "PASV") |> should.be_true
    dict.has_key(features, "SIZE") |> should.be_true
    dict.has_key(features, "UTF8") |> should.be_true
    // REST has a value "STREAM"
    dict.get(features, "REST") |> should.equal(Ok(Some("STREAM")))
    Nil
  })
}

// --- Misc command tests ---

pub fn opts_utf8_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.opts(client, "UTF8", Some("ON"))
    Nil
  })
}

pub fn site_test() {
  with_ftp_connection(fn(client) {
    // SITE HELP is a commonly supported sub-command
    let assert Ok(_resp) = gftp.site(client, "HELP")
    Nil
  })
}

pub fn custom_command_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(resp) = gftp.custom_command(client, "SYST", [status.System])
    let assert Ok(body) = response.to_string(resp)
    // vsftpd typically returns "UNIX Type: L8"
    body |> string.contains("UNIX") |> should.be_true
    Nil
  })
}

pub fn rest_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    let content = "0123456789ABCDEF"
    let assert Ok(_) =
      gftp.stor(client, "rest_test.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string(content))
        |> result.map_error(ftp_result.Socket)
      })
    // Set restart offset to 10 bytes
    let assert Ok(_) = gftp.rest(client, 10)
    // Retrieve with offset — should only get "ABCDEF"
    let assert Ok(_) =
      gftp.retr(client, "rest_test.txt", fn(data_stream) {
        let assert Ok(data) = stream.receive(data_stream, 5000)
        let assert Ok(text) = bit_array.to_string(data)
        text |> should.equal("ABCDEF")
        Ok(Nil)
      })
    let assert Ok(_) = gftp.dele(client, "rest_test.txt")
    Nil
  })
}

// --- FTPS tests ---

/// Run an integration test with a real FTPS connection.
/// Starts a Docker FTP container with TLS enabled, connects,
/// upgrades to TLS via AUTH TLS, logs in, runs the callback, then cleans up.
/// Skipped unless GFTP_INTEGRATION_TESTS env var is set.
fn with_ftps_connection(callback: fn(FtpClient) -> Nil) -> Nil {
  case require_integration() {
    False -> Nil
    True -> {
      let container_id = start_ftps_container()
      let port = get_ftps_port(container_id)
      let assert Ok(client) =
        gftp.connect_timeout("127.0.0.1", port, timeout: 30_000)
      let ssl_options =
        kafein.default_options
        |> kafein.verify(verify_type: kafein.VerifyNone)
      let assert Ok(client) = gftp.into_secure(client, ssl_options)
      let assert Ok(_) = gftp.login(client, "test", "test")
      callback(client)
      let assert Ok(_) = gftp.quit(client)
      stop_container(container_id)
    }
  }
}

pub fn ftps_connect_and_login_test() {
  with_ftps_connection(fn(client) {
    let assert Ok(pwd) = gftp.pwd(client)
    pwd |> should.equal("/home/test")
    Nil
  })
}

pub fn ftps_stor_and_retr_test() {
  with_ftps_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    let content = "Hello, FTPS!"
    let assert Ok(_) =
      gftp.stor(client, "ftps_test.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string(content))
        |> result.map_error(ftp_result.Socket)
      })
    let assert Ok(_) =
      gftp.retr(client, "ftps_test.txt", fn(data_stream) {
        let assert Ok(data) = stream.receive(data_stream, 5000)
        let assert Ok(text) = bit_array.to_string(data)
        text |> should.equal(content)
        Ok(Nil)
      })
    let assert Ok(_) = gftp.dele(client, "ftps_test.txt")
    Nil
  })
}
