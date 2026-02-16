//// Internal module for opening and closing FTP data channels.
////
//// These functions are used by `gftp/actor` and are not part of the public API.

import gftp.{type FtpClient}
import gftp/internal/command
import gftp/response.{type Response}
import gftp/result.{type FtpError} as _ftp_result
import gftp/status.{type Status}
import gftp/stream.{type DataStream}
import gleam/option.{type Option}
import gleam/result

/// Open a data channel for downloading a file.
pub fn open_retr(
  ftp_client: FtpClient,
  path: String,
) -> Result(DataStream, FtpError) {
  use data_stream <- result.try(gftp.data_command(
    ftp_client,
    command.Retr(path),
  ))
  use _ <- result.try(
    gftp.read_response_in(ftp_client, [
      status.AboutToSend,
      status.AlreadyOpen,
    ]),
  )
  Ok(data_stream)
}

/// Open a data channel for uploading a file.
pub fn open_stor(
  ftp_client: FtpClient,
  path: String,
) -> Result(DataStream, FtpError) {
  use data_stream <- result.try(gftp.data_command(
    ftp_client,
    command.Stor(path),
  ))
  use _ <- result.try(
    gftp.read_response_in(ftp_client, [
      status.AboutToSend,
      status.AlreadyOpen,
    ]),
  )
  Ok(data_stream)
}

/// Open a data channel for appending to a file.
pub fn open_appe(
  ftp_client: FtpClient,
  path: String,
) -> Result(DataStream, FtpError) {
  use data_stream <- result.try(gftp.data_command(
    ftp_client,
    command.Appe(path),
  ))
  use _ <- result.try(
    gftp.read_response_in(ftp_client, [
      status.AboutToSend,
      status.AlreadyOpen,
    ]),
  )
  Ok(data_stream)
}

/// Open a data channel for a LIST directory listing.
pub fn open_list(
  ftp_client: FtpClient,
  pathname: Option(String),
) -> Result(DataStream, FtpError) {
  use data_stream <- result.try(gftp.data_command(
    ftp_client,
    command.List(pathname),
  ))
  use _ <- result.try(
    gftp.read_response_in(ftp_client, [
      status.AboutToSend,
      status.AlreadyOpen,
    ]),
  )
  Ok(data_stream)
}

/// Open a data channel for an NLST file name listing.
pub fn open_nlst(
  ftp_client: FtpClient,
  pathname: Option(String),
) -> Result(DataStream, FtpError) {
  use data_stream <- result.try(gftp.data_command(
    ftp_client,
    command.Nlst(pathname),
  ))
  use _ <- result.try(
    gftp.read_response_in(ftp_client, [
      status.AboutToSend,
      status.AlreadyOpen,
    ]),
  )
  Ok(data_stream)
}

/// Open a data channel for an MLSD machine-readable listing.
pub fn open_mlsd(
  ftp_client: FtpClient,
  pathname: Option(String),
) -> Result(DataStream, FtpError) {
  use data_stream <- result.try(gftp.data_command(
    ftp_client,
    command.Mlsd(pathname),
  ))
  use _ <- result.try(
    gftp.read_response_in(ftp_client, [
      status.AboutToSend,
      status.AlreadyOpen,
    ]),
  )
  Ok(data_stream)
}

/// Open a data channel for a custom FTP command.
pub fn open_data_command(
  ftp_client: FtpClient,
  command_str: String,
  expected_statuses: List(Status),
) -> Result(#(DataStream, Response), FtpError) {
  use data_stream <- result.try(gftp.data_command(
    ftp_client,
    command.Custom(command_str),
  ))
  use response <- result.try(gftp.read_response_in(
    ftp_client,
    expected_statuses,
  ))
  Ok(#(data_stream, response))
}

/// Close a data channel and finalize the transfer.
pub fn close_data_channel(
  ftp_client: FtpClient,
  data_stream: DataStream,
) -> Result(Nil, FtpError) {
  gftp.finalize_data_command(ftp_client, data_stream)
}
