import envoy
import gftp
import gftp/file_type
import gftp/result as ftp_result
import gftp/stream
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None}
import gleam/result

pub fn main() {
  // Read connection details from environment variables
  let assert Ok(host) = envoy.get("FTP_HOST")
  let port = envoy.get("FTP_PORT") |> result.try(int.parse) |> result.unwrap(21)
  let assert Ok(user) = envoy.get("FTP_USER")
  let assert Ok(password) = envoy.get("FTP_PASSWORD")

  io.println("Connecting to " <> host <> ":" <> int.to_string(port) <> "...")

  // Connect and login
  let assert Ok(client) = gftp.connect_timeout(host, port, timeout: 10_000)
  io.println("Connected! Logging in as " <> user <> "...")
  let assert Ok(_) = gftp.login(client, user, password)
  io.println("Logged in.")

  // Print working directory
  let assert Ok(cwd) = gftp.pwd(client)
  io.println("Current directory: " <> cwd)

  // Set binary transfer type
  let assert Ok(_) = gftp.transfer_type(client, file_type.Binary)

  // List current directory
  let assert Ok(entries) = gftp.list(client, None)
  io.println(
    "Directory listing (" <> int.to_string(list.length(entries)) <> " entries):",
  )
  list.each(entries, fn(entry) { io.println("  " <> entry) })

  // Upload a small file
  let file_content = "Hello from gftp simple_client example!"
  io.println("\nUploading gftp_example.txt...")
  let assert Ok(_) =
    gftp.stor(client, "gftp_example.txt", fn(data_stream) {
      stream.send(data_stream, bit_array.from_string(file_content))
      |> result.map_error(ftp_result.Socket)
    })
  io.println("Upload complete.")

  // Download it back
  io.println("Downloading gftp_example.txt...")
  let assert Ok(_) =
    gftp.retr(client, "gftp_example.txt", fn(data_stream) {
      case stream.receive(data_stream, 5000) {
        Ok(data) -> {
          let assert Ok(text) = bit_array.to_string(data)
          io.println("Downloaded content: " <> text)
          Ok(Nil)
        }
        Error(e) -> Error(ftp_result.Socket(e))
      }
    })

  // Clean up - delete the uploaded file
  io.println("Deleting gftp_example.txt...")
  let assert Ok(_) = gftp.dele(client, "gftp_example.txt")
  io.println("Deleted.")

  // Disconnect
  let assert Ok(_) = gftp.quit(client)
  let _ = gftp.shutdown(client)
  io.println("Disconnected. Done!")
}
