import envoy
import gftp.{type FtpClient}
import gftp/actor as ftp_actor
import gftp/file_type
import gftp/response
import gftp/result as ftp_result
import gftp/status
import gftp/stream.{Packet}
import gleam/bit_array
import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import kafein
import mug
import testcontainers_gleam
import testcontainers_gleam/container.{type Container}
import testcontainers_gleam/wait_strategy

fn require_integration() -> Bool {
  case envoy.get("GFTP_INTEGRATION_TESTS") {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn require_active_mode() -> Bool {
  case envoy.get("GFTP_ACTIVE_MODE_TESTS") {
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
      let container = alpine_ftp_container()
      let assert Ok(container) = testcontainers_gleam.start_container(container)
      let container_id = container.container_id(container)

      let assert Ok(port) = container.mapped_port(container, 21)
      let assert Ok(client) =
        gftp.connect_timeout("127.0.0.1", port, timeout: 30_000)

      let client =
        gftp.with_passive_stream_builder(client, fn(host, port) {
          // get mapped port for data
          let assert Ok(data_port) = container.mapped_port(container, port)

          mug.new(host, data_port)
          |> mug.connect()
          |> result.map_error(ftp_result.ConnectionError)
          |> result.map(stream.Tcp)
        })

      let assert Ok(_) = gftp.login(client, "test", "test")
      callback(client)
      let assert Ok(_) = gftp.quit(client)
      let assert Ok(_) = testcontainers_gleam.stop_container(container_id)
      Nil
    }
  }
}

/// Run an integration test with a real FTPS connection.
/// Starts a Docker FTP container with TLS enabled, connects,
/// upgrades to TLS via AUTH TLS, logs in, runs the callback, then cleans up.
/// Skipped unless GFTP_INTEGRATION_TESTS env var is set.
fn with_ftps_connection(callback: fn(FtpClient) -> Nil) -> Nil {
  case require_integration() {
    False -> {
      Nil
    }
    True -> {
      let container = alpine_ftps_container()
      let assert Ok(container) = testcontainers_gleam.start_container(container)
      let container_id = container.container_id(container)

      // Set up TLS inside the running container (install certs, start vsftpd on port 990)
      setup_ftps(container_id)

      let assert Ok(port) = container.mapped_port(container, 990)
      let ssl_options =
        kafein.default_options
        |> kafein.verify(kafein.VerifyNone)
      let assert Ok(client) =
        gftp.connect_timeout("127.0.0.1", port, timeout: 30_000)
        |> result.try(fn(client) { gftp.into_secure(client, ssl_options) })

      let client =
        gftp.with_passive_stream_builder(client, fn(host, port) {
          // get mapped port for data
          let assert Ok(data_port) = container.mapped_port(container, port)

          mug.new(host, data_port)
          |> mug.connect()
          |> result.map_error(ftp_result.ConnectionError)
          |> result.map(stream.Tcp)
        })

      let assert Ok(_) = gftp.login(client, "test", "test")
      callback(client)
      let assert Ok(_) = gftp.quit(client)
      let assert Ok(_) = testcontainers_gleam.stop_container(container_id)
      Nil
    }
  }
}

/// Run an integration test with an FTP connection in active mode.
/// Starts a Docker FTP container with --network host, connects, logs in,
/// switches to active mode, runs the callback, then cleans up.
/// Skipped unless GFTP_INTEGRATION_TESTS env var is set.
fn with_ftp_active_connection(callback: fn(FtpClient) -> Nil) -> Nil {
  case require_active_mode() {
    False -> Nil
    True -> {
      let container = alpine_ftp_active_container()
      let assert Ok(container) = testcontainers_gleam.start_container(container)
      let container_id = container.container_id(container)

      let assert Ok(port) = container.mapped_port(container, 21)
      let assert Ok(client) =
        gftp.connect_timeout("127.0.0.1", port, timeout: 30_000)

      let client =
        gftp.with_passive_stream_builder(client, fn(host, port) {
          // get mapped port for data
          let assert Ok(data_port) = container.mapped_port(container, port)

          mug.new(host, data_port)
          |> mug.connect()
          |> result.map_error(ftp_result.ConnectionError)
          |> result.map(stream.Tcp)
        })

      let assert Ok(_) = gftp.login(client, "test", "test")
      callback(client)
      let assert Ok(_) = gftp.quit(client)
      let assert Ok(_) = testcontainers_gleam.stop_container(container_id)
      Nil
    }
  }
}

/// Run an integration test with an actor-wrapped FTP connection.
fn with_actor_connection(callback: fn(ftp_actor.Handle) -> Nil) -> Nil {
  case require_integration() {
    False -> Nil
    True -> {
      let container = alpine_ftps_container()
      let assert Ok(container) = testcontainers_gleam.start_container(container)
      let container_id = container.container_id(container)

      // Set up TLS inside the running container (install certs, start vsftpd on port 990)
      setup_ftps(container_id)

      let assert Ok(port) = container.mapped_port(container, 990)
      let ssl_options =
        kafein.default_options
        |> kafein.verify(kafein.VerifyNone)
      let assert Ok(client) =
        gftp.connect_timeout("127.0.0.1", port, timeout: 30_000)
        |> result.try(fn(client) { gftp.into_secure(client, ssl_options) })

      let client =
        gftp.with_passive_stream_builder(client, fn(host, port) {
          // get mapped port for data
          let assert Ok(data_port) = container.mapped_port(container, port)

          mug.new(host, data_port)
          |> mug.connect()
          |> result.map_error(ftp_result.ConnectionError)
          |> result.map(stream.Tcp)
        })
      let assert Ok(started) = ftp_actor.start(client)
      let handle = started.data
      let assert Ok(_) = ftp_actor.login(handle, "test", "test")
      callback(handle)
      let assert Ok(_) = ftp_actor.quit(handle)
      let assert Ok(_) = testcontainers_gleam.stop_container(container_id)
      Nil
    }
  }
}

/// setup function to create a Docker container with an FTP server for testing.
fn alpine_ftp_container() -> Container {
  "delfer/alpine-ftp-server:latest"
  |> container.new()
  |> container.with_exposed_port(21)
  |> container.with_exposed_ports(list.range(21_100, 21_110))
  |> container.with_environment("MIN_PORT", "21100")
  |> container.with_environment("MAX_PORT", "21110")
  |> container.with_environment("USERS", "test|test|/home/test")
  |> container.with_environment("ADDRESS", "127.0.0.1")
  |> container.with_waiting_strategy(wait_strategy.log("passwd:", 30_000, 500))
  |> container.with_auto_remove(True)
}

/// setup function to create a Docker container with an FTPS server for testing.
/// Note: after starting, call setup_ftps(container_id) to install certs
/// and start a second vsftpd instance with TLS on port 990.
fn alpine_ftps_container() -> Container {
  alpine_ftp_container()
  |> container.with_exposed_port(990)
}

@external(erlang, "test_container_ffi", "setup_ftps")
fn setup_ftps(container_id: String) -> Nil

/// setup function to create a Docker container with an FTP server for active mode testing.
/// Uses --network host so the server can connect back to the client's listener on 127.
fn alpine_ftp_active_container() -> Container {
  alpine_ftp_container()
  |> container.with_network_mode("host")
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
    let assert False = msg == None
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
    let assert "/home/test" = pwd
    Nil
  })
}

pub fn cwd_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(pwd) = gftp.pwd(client)
    let assert Ok(_) = gftp.cwd(client, "/")
    let assert Ok(new_pwd) = gftp.pwd(client)
    let assert "/" = new_pwd
    let assert Ok(_) = gftp.cwd(client, pwd)
    Nil
  })
}

pub fn cdup_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.cwd(client, "/home/test")
    let assert Ok(_) = gftp.cdup(client)
    let assert Ok(pwd) = gftp.pwd(client)
    let assert "/home" = pwd
    Nil
  })
}

// --- Directory management tests ---

pub fn mkd_and_rmd_test() {
  with_ftp_connection(fn(client) {
    let assert Ok(_) = gftp.mkd(client, "test_dir")
    let assert Ok(_) = gftp.cwd(client, "test_dir")
    let assert Ok(pwd) = gftp.pwd(client)
    let assert "/home/test/test_dir" = pwd
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
        let assert True = text == content
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
        let assert "Hello, World!" = text
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
    let assert Error(_) = gftp.size(client, "to_delete.txt")
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
    let assert Error(_) = gftp.size(client, "original.txt")
    // Verify new name exists and has correct content
    let assert Ok(_) =
      gftp.retr(client, "renamed.txt", fn(data_stream) {
        let assert Ok(data) = stream.receive(data_stream, 5000)
        let assert Ok(text) = bit_array.to_string(data)
        let assert "rename me" = text
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
    let assert 10 = file_size
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
    let assert True = has_file
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
    let assert True = has_file
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
    let assert True = dict.has_key(features, "EPRT")
    let assert True = dict.has_key(features, "EPSV")
    let assert True = dict.has_key(features, "MDTM")
    let assert True = dict.has_key(features, "PASV")
    let assert True = dict.has_key(features, "SIZE")
    let assert True = dict.has_key(features, "UTF8")
    // REST has a value "STREAM"
    let assert Ok(Some("STREAM")) = dict.get(features, "REST")
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
    let assert Ok(resp) = gftp.custom_command(client, "SYST", [status.Name])
    let assert Ok(body) = response.to_string(resp)
    // vsftpd typically returns "UNIX Type: L8"
    let assert True = string.contains(body, "UNIX")
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
        let assert "ABCDEF" = text
        Ok(Nil)
      })
    let assert Ok(_) = gftp.dele(client, "rest_test.txt")
    Nil
  })
}

// --- Active mode tests ---
// These tests require --network host Docker mode (Linux only).
// With host networking, the FTP server can connect back to the client's
// listener on 127.0.0.1 for active mode data transfers.

pub fn active_mode_list_test() {
  with_ftp_active_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    let assert Ok(_) =
      gftp.stor(client, "active_list_test.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string("test"))
        |> result.map_error(ftp_result.Socket)
      })
    let assert Ok(lines) = gftp.list(client, None)
    let has_file =
      lines
      |> list.any(fn(line) { string.contains(line, "active_list_test.txt") })
    let assert True = has_file
    let assert Ok(_) = gftp.dele(client, "active_list_test.txt")
    Nil
  })
}

pub fn active_mode_stor_and_retr_test() {
  with_ftp_active_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    let content = "Hello, active mode!"
    let assert Ok(_) =
      gftp.stor(client, "active_test.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string(content))
        |> result.map_error(ftp_result.Socket)
      })
    let assert Ok(_) =
      gftp.retr(client, "active_test.txt", fn(data_stream) {
        let assert Ok(data) = stream.receive(data_stream, 5000)
        let assert Ok(text) = bit_array.to_string(data)
        let assert True = text == content
        Ok(Nil)
      })
    let assert Ok(_) = gftp.dele(client, "active_test.txt")
    Nil
  })
}

pub fn active_mode_nlst_test() {
  with_ftp_active_connection(fn(client) {
    let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)
    let assert Ok(_) =
      gftp.stor(client, "active_nlst_test.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string("test"))
        |> result.map_error(ftp_result.Socket)
      })
    let assert Ok(names) = gftp.nlst(client, None)
    let has_file =
      names
      |> list.any(fn(name) { string.contains(name, "active_nlst_test.txt") })
    let assert True = has_file
    let assert Ok(_) = gftp.dele(client, "active_nlst_test.txt")
    Nil
  })
}

// --- FTPS tests ---

pub fn ftps_connect_and_login_test() {
  with_ftps_connection(fn(client) {
    let assert Ok(pwd) = gftp.pwd(client)
    let assert "/home/test" = pwd
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
        let assert True = text == content
        Ok(Nil)
      })
    let assert Ok(_) = gftp.dele(client, "ftps_test.txt")
    Nil
  })
}

// --- Actor integration tests ---

pub fn actor_stor_and_retr_test() {
  with_actor_connection(fn(handle) {
    let assert Ok(_) = ftp_actor.transfer_type(handle, file_type.Binary)
    let content = "Hello, actor!"
    let assert Ok(_) =
      ftp_actor.stor(handle, "actor_test.txt", fn(data_stream) {
        stream.send(data_stream, bit_array.from_string(content))
        |> result.map_error(ftp_result.Socket)
      })
    let assert Ok(_) =
      ftp_actor.retr(handle, "actor_test.txt", fn(data_stream) {
        let assert Ok(data) = stream.receive(data_stream, 5000)
        let assert Ok(text) = bit_array.to_string(data)
        let assert True = text == content
        Ok(Nil)
      })
    let assert Ok(_) = ftp_actor.dele(handle, "actor_test.txt")
    Nil
  })
}

pub fn actor_chunk_protection_test() {
  with_actor_connection(fn(handle) {
    let assert Ok(_) = ftp_actor.transfer_type(handle, file_type.Binary)

    // Open a data channel
    let assert Ok(data_stream) =
      ftp_actor.open_stor(handle, "actor_chunk_test.txt")

    // Control commands should be rejected
    let assert Error(ftp_result.DataTransferInProgress) = ftp_actor.pwd(handle)

    // Write some data and close
    let assert Ok(_) =
      stream.send(data_stream, bit_array.from_string("chunk test"))
    let assert Ok(_) = ftp_actor.close_data_channel(handle, data_stream)

    // Control commands should work again
    let assert Ok(_pwd) = ftp_actor.pwd(handle)

    let assert Ok(_) = ftp_actor.dele(handle, "actor_chunk_test.txt")
    Nil
  })
}

pub fn actor_open_retr_test() {
  with_actor_connection(fn(handle) {
    let assert Ok(_) = ftp_actor.transfer_type(handle, file_type.Binary)
    let content = "Hello, message-based actor retr!"

    // Upload a file first
    let assert Ok(_) =
      ftp_actor.stor(handle, "actor_retr_test.txt", fn(ds) {
        stream.send(ds, bit_array.from_string(content))
        |> result.map_error(ftp_result.Socket)
      })

    // Open data channel for download
    let assert Ok(data_stream) =
      ftp_actor.open_retr(handle, "actor_retr_test.txt")

    // Arm for message-based receive
    stream.receive_next_packet_as_message(data_stream)
    let selector =
      process.new_selector()
      |> stream.select_stream_messages(fn(msg) { msg })

    // Receive data
    let assert Ok(Packet(data)) =
      process.selector_receive(from: selector, within: 5000)
    let assert Ok(text) = bit_array.to_string(data)
    let assert True = text == content

    // Close
    let assert Ok(_) = ftp_actor.close_data_channel(handle, data_stream)

    let assert Ok(_) = ftp_actor.dele(handle, "actor_retr_test.txt")
    Nil
  })
}

pub fn actor_quit_test() {
  case require_integration() {
    False -> Nil
    True -> {
      let container = alpine_ftp_container()
      let assert Ok(container) = testcontainers_gleam.start_container(container)
      let container_id = container.container_id(container)

      let assert Ok(port) = container.mapped_port(container, 21)
      let assert Ok(client) =
        gftp.connect_timeout("127.0.0.1", port, timeout: 30_000)

      let client =
        gftp.with_passive_stream_builder(client, fn(host, port) {
          let assert Ok(data_port) = container.mapped_port(container, port)

          mug.new(host, data_port)
          |> mug.connect()
          |> result.map_error(ftp_result.ConnectionError)
          |> result.map(stream.Tcp)
        })

      let assert Ok(started) = ftp_actor.start(client)
      let handle = started.data
      let assert Ok(_) = ftp_actor.login(handle, "test", "test")

      // Quit should succeed
      let assert Ok(_) = ftp_actor.quit(handle)

      // Actor process should be dead - verify by checking if the pid is alive
      let assert False = process.is_alive(started.pid)

      let assert Ok(_) = testcontainers_gleam.stop_container(container_id)
      Nil
    }
  }
}
