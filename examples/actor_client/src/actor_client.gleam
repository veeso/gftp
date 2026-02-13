import envoy
import gftp
import gftp/actor as ftp_actor
import gftp/file_type
import gftp/result as ftp_result
import gftp/stream.{type StreamMessage, Packet, StreamClosed, StreamError}
import gleam/bit_array
import gleam/bytes_tree
import gleam/erlang/process
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

  // Connect and wrap in an OTP actor
  let assert Ok(client) = gftp.connect_timeout(host, port, timeout: 10_000)
  let assert Ok(started) = ftp_actor.start(client)
  let handle = started.data
  io.println("Connected and actor started.")

  // Login via actor
  let assert Ok(_) = ftp_actor.login(handle, user, password)
  io.println("Logged in as " <> user <> ".")

  // Print working directory
  let assert Ok(cwd) = ftp_actor.pwd(handle)
  io.println("Current directory: " <> cwd)

  // Set binary transfer type
  let assert Ok(_) = ftp_actor.transfer_type(handle, file_type.Binary)

  // List directory
  let assert Ok(entries) = ftp_actor.list(handle, None)
  io.println(
    "Directory listing (" <> int.to_string(list.length(entries)) <> " entries):",
  )
  list.each(entries, fn(entry) { io.println("  " <> entry) })

  // Upload using message-based streaming (open_stor)
  let file_content = "Hello from gftp actor_client example!"
  io.println(
    "\nUploading gftp_actor_example.txt via message-based streaming...",
  )
  let assert Ok(upload_stream) =
    ftp_actor.open_stor(handle, "gftp_actor_example.txt")
  let assert Ok(_) =
    stream.send(upload_stream, bit_array.from_string(file_content))
    |> result.map_error(ftp_result.Socket)
  let assert Ok(_) = ftp_actor.close_data_channel(handle, upload_stream)
  io.println("Upload complete.")

  // Demonstrate chunk protection: try a control command while data channel is open
  io.println("\nDemonstrating chunk protection...")
  let assert Ok(data_stream) =
    ftp_actor.open_retr(handle, "gftp_actor_example.txt")
  case ftp_actor.pwd(handle) {
    Error(ftp_result.DataTransferInProgress) ->
      io.println(
        "  pwd() correctly returned DataTransferInProgress"
        <> " while data channel is open.",
      )
    other ->
      io.println(
        "  Unexpected result from pwd(): "
        <> case other {
          Ok(p) -> "Ok(" <> p <> ")"
          Error(e) -> "Error(" <> ftp_result.describe_error(e) <> ")"
        },
      )
  }

  // Download the file via message-based streaming
  io.println(
    "\nDownloading gftp_actor_example.txt via message-based streaming...",
  )
  stream.receive_next_packet_as_message(data_stream)
  let selector =
    process.new_selector()
    |> stream.select_stream_messages(fn(msg) { msg })
  let downloaded = receive_loop(selector, data_stream, bytes_tree.new())
  let assert Ok(text) = bit_array.to_string(bytes_tree.to_bit_array(downloaded))
  io.println("Downloaded content: " <> text)

  // Close the data channel
  let assert Ok(_) = ftp_actor.close_data_channel(handle, data_stream)

  // Now control commands work again
  let assert Ok(cwd) = ftp_actor.pwd(handle)
  io.println("pwd() works again after closing data channel: " <> cwd)

  // Clean up
  io.println("\nDeleting gftp_actor_example.txt...")
  let assert Ok(_) = ftp_actor.dele(handle, "gftp_actor_example.txt")
  io.println("Deleted.")

  // Quit (stops the actor too)
  let assert Ok(_) = ftp_actor.quit(handle)
  io.println("Disconnected. Done!")
}

fn receive_loop(
  selector: process.Selector(StreamMessage),
  data_stream: stream.DataStream,
  acc: bytes_tree.BytesTree,
) -> bytes_tree.BytesTree {
  case process.selector_receive(selector, 10_000) {
    Ok(Packet(data)) -> {
      stream.receive_next_packet_as_message(data_stream)
      receive_loop(selector, data_stream, bytes_tree.append(acc, data))
    }
    Ok(StreamClosed) -> acc
    Ok(StreamError(err)) -> {
      io.println("Stream error: " <> ftp_result.describe_error(err))
      acc
    }
    Error(_) -> {
      io.println("Timeout waiting for data.")
      acc
    }
  }
}
