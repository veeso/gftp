//// FTP Response returned by the server.

import gftp/status
import gleam/bit_array
import gleam/result

/// The Response type represents a response from the FTP server. It contains a status code and a message.
pub type Response {
  Response(code: status.Status, message: BitArray)
}

/// The message_to_string function takes a BitArray and converts it to a String.
/// If the BitArray is not valid UTF-8, it returns an error message.
fn message_to_string(message: BitArray) -> String {
  message
  |> bit_array.to_string
  |> result.unwrap(or: "Invalid UTF-8 in response message")
}

/// The describe function takes a Response and returns a human-readable string describing the response.
pub fn describe(response: Response) -> String {
  "["
  <> status.describe(response.code)
  <> "] "
  <> message_to_string(response.message)
}

pub fn to_string(response: Response) -> Result(String, Nil) {
  bit_array.to_string(response.message)
}
