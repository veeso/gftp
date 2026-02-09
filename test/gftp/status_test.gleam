import gftp/status.{
  AboutToSend, ActionAborted, AlreadyOpen, AuthOk, BadArguments, BadCommand,
  BadFilename, BadSequence, CannotOpenDataConnection, Closing,
  ClosingDataConnection, CommandNotImplemented, CommandOk, DataConnectionOpen,
  Directory, ExceededStorage, ExtendedPassiveMode, File, FileUnavailable, Help,
  HostUnavailable, InvalidCredentials, LoggedIn, LoggedOut, LoginNeedAccount,
  LogoutAck, LongPassiveMode, Name, NeedPassword, NotAvailable, NotImplemented,
  NotImplementedParameter, NotLoggedIn, PageTypeUnknown, PassiveMode,
  PathCreated, Ready, ReadyMinute, RequestFileActionIgnored, RequestFilePending,
  RequestedActionNotTaken, RequestedFileActionOk, RestartMarker,
  StoringNeedAccount, System, TransferAborted, Unknown,
}
import gleeunit/should

// --- from_int tests ---

pub fn from_int_restart_marker_test() {
  status.from_int(110)
  |> should.equal(RestartMarker)
}

pub fn from_int_ready_minute_test() {
  status.from_int(120)
  |> should.equal(ReadyMinute)
}

pub fn from_int_already_open_test() {
  status.from_int(125)
  |> should.equal(AlreadyOpen)
}

pub fn from_int_about_to_send_test() {
  status.from_int(150)
  |> should.equal(AboutToSend)
}

pub fn from_int_command_ok_test() {
  status.from_int(200)
  |> should.equal(CommandOk)
}

pub fn from_int_command_not_implemented_test() {
  status.from_int(202)
  |> should.equal(CommandNotImplemented)
}

pub fn from_int_system_test() {
  status.from_int(211)
  |> should.equal(System)
}

pub fn from_int_directory_test() {
  status.from_int(212)
  |> should.equal(Directory)
}

pub fn from_int_file_test() {
  status.from_int(213)
  |> should.equal(File)
}

pub fn from_int_help_test() {
  status.from_int(214)
  |> should.equal(Help)
}

pub fn from_int_name_test() {
  status.from_int(215)
  |> should.equal(Name)
}

pub fn from_int_ready_test() {
  status.from_int(220)
  |> should.equal(Ready)
}

pub fn from_int_closing_test() {
  status.from_int(221)
  |> should.equal(Closing)
}

pub fn from_int_data_connection_open_test() {
  status.from_int(225)
  |> should.equal(DataConnectionOpen)
}

pub fn from_int_closing_data_connection_test() {
  status.from_int(226)
  |> should.equal(ClosingDataConnection)
}

pub fn from_int_passive_mode_test() {
  status.from_int(227)
  |> should.equal(PassiveMode)
}

pub fn from_int_long_passive_mode_test() {
  status.from_int(228)
  |> should.equal(LongPassiveMode)
}

pub fn from_int_extended_passive_mode_test() {
  status.from_int(229)
  |> should.equal(ExtendedPassiveMode)
}

pub fn from_int_logged_in_test() {
  status.from_int(230)
  |> should.equal(LoggedIn)
}

pub fn from_int_logged_out_test() {
  status.from_int(231)
  |> should.equal(LoggedOut)
}

pub fn from_int_logout_ack_test() {
  status.from_int(232)
  |> should.equal(LogoutAck)
}

pub fn from_int_auth_ok_test() {
  status.from_int(234)
  |> should.equal(AuthOk)
}

pub fn from_int_requested_file_action_ok_test() {
  status.from_int(250)
  |> should.equal(RequestedFileActionOk)
}

pub fn from_int_path_created_test() {
  status.from_int(257)
  |> should.equal(PathCreated)
}

pub fn from_int_need_password_test() {
  status.from_int(331)
  |> should.equal(NeedPassword)
}

pub fn from_int_login_need_account_test() {
  status.from_int(332)
  |> should.equal(LoginNeedAccount)
}

pub fn from_int_request_file_pending_test() {
  status.from_int(350)
  |> should.equal(RequestFilePending)
}

pub fn from_int_not_available_test() {
  status.from_int(421)
  |> should.equal(NotAvailable)
}

pub fn from_int_cannot_open_data_connection_test() {
  status.from_int(425)
  |> should.equal(CannotOpenDataConnection)
}

pub fn from_int_transfer_aborted_test() {
  status.from_int(426)
  |> should.equal(TransferAborted)
}

pub fn from_int_invalid_credentials_test() {
  status.from_int(430)
  |> should.equal(InvalidCredentials)
}

pub fn from_int_host_unavailable_test() {
  status.from_int(434)
  |> should.equal(HostUnavailable)
}

pub fn from_int_request_file_action_ignored_test() {
  status.from_int(450)
  |> should.equal(RequestFileActionIgnored)
}

pub fn from_int_action_aborted_test() {
  status.from_int(451)
  |> should.equal(ActionAborted)
}

pub fn from_int_requested_action_not_taken_test() {
  status.from_int(452)
  |> should.equal(RequestedActionNotTaken)
}

pub fn from_int_bad_command_test() {
  status.from_int(500)
  |> should.equal(BadCommand)
}

pub fn from_int_bad_arguments_test() {
  status.from_int(501)
  |> should.equal(BadArguments)
}

pub fn from_int_not_implemented_test() {
  status.from_int(502)
  |> should.equal(NotImplemented)
}

pub fn from_int_bad_sequence_test() {
  status.from_int(503)
  |> should.equal(BadSequence)
}

pub fn from_int_not_implemented_parameter_test() {
  status.from_int(504)
  |> should.equal(NotImplementedParameter)
}

pub fn from_int_not_logged_in_test() {
  status.from_int(530)
  |> should.equal(NotLoggedIn)
}

pub fn from_int_storing_need_account_test() {
  status.from_int(532)
  |> should.equal(StoringNeedAccount)
}

pub fn from_int_file_unavailable_test() {
  status.from_int(550)
  |> should.equal(FileUnavailable)
}

pub fn from_int_page_type_unknown_test() {
  status.from_int(551)
  |> should.equal(PageTypeUnknown)
}

pub fn from_int_exceeded_storage_test() {
  status.from_int(552)
  |> should.equal(ExceededStorage)
}

pub fn from_int_bad_filename_test() {
  status.from_int(553)
  |> should.equal(BadFilename)
}

pub fn from_int_unknown_test() {
  status.from_int(999)
  |> should.equal(Unknown)
}

// --- description tests ---

pub fn description_restart_marker_test() {
  status.describe_error(RestartMarker)
  |> should.equal("restart marker reply")
}

pub fn description_ready_minute_test() {
  status.describe_error(ReadyMinute)
  |> should.equal("service ready in (n) minutes")
}

pub fn description_already_open_test() {
  status.describe_error(AlreadyOpen)
  |> should.equal("data connection already open transfer starting")
}

pub fn description_about_to_send_test() {
  status.describe_error(AboutToSend)
  |> should.equal("file status okay about to open data connection")
}

pub fn description_command_ok_test() {
  status.describe_error(CommandOk)
  |> should.equal("command okay")
}

pub fn description_command_not_implemented_test() {
  status.describe_error(CommandNotImplemented)
  |> should.equal("command not implemented")
}

pub fn description_system_test() {
  status.describe_error(System)
  |> should.equal("system status or system help reply")
}

pub fn description_directory_test() {
  status.describe_error(Directory)
  |> should.equal("directory status")
}

pub fn description_file_test() {
  status.describe_error(File)
  |> should.equal("file status")
}

pub fn description_help_test() {
  status.describe_error(Help)
  |> should.equal("help message")
}

pub fn description_name_test() {
  status.describe_error(Name)
  |> should.equal("NAME system type")
}

pub fn description_ready_test() {
  status.describe_error(Ready)
  |> should.equal("service ready for new user")
}

pub fn description_closing_test() {
  status.describe_error(Closing)
  |> should.equal("service closing control connection")
}

pub fn description_data_connection_open_test() {
  status.describe_error(DataConnectionOpen)
  |> should.equal("data connection open; no transfer in progress")
}

pub fn description_closing_data_connection_test() {
  status.describe_error(ClosingDataConnection)
  |> should.equal("closing data connection")
}

pub fn description_passive_mode_test() {
  status.describe_error(PassiveMode)
  |> should.equal("entering passive mode")
}

pub fn description_long_passive_mode_test() {
  status.describe_error(LongPassiveMode)
  |> should.equal("entering long passive mode")
}

pub fn description_extended_passive_mode_test() {
  status.describe_error(ExtendedPassiveMode)
  |> should.equal("entering extended passive mode")
}

pub fn description_logged_in_test() {
  status.describe_error(LoggedIn)
  |> should.equal("user logged in proceed. Logged out if appropriate.")
}

pub fn description_logged_out_test() {
  status.describe_error(LoggedOut)
  |> should.equal("user logged out; service terminated")
}

pub fn description_logout_ack_test() {
  status.describe_error(LogoutAck)
  |> should.equal("logout command noted will complete when transfer done")
}

pub fn description_auth_ok_test() {
  status.describe_error(AuthOk)
  |> should.equal(
    "specifies that the server accepts the authentication mechanism specified by the client",
  )
}

pub fn description_requested_file_action_ok_test() {
  status.describe_error(RequestedFileActionOk)
  |> should.equal("requested file action okay")
}

pub fn description_path_created_test() {
  status.describe_error(PathCreated)
  |> should.equal("pathname created")
}

pub fn description_need_password_test() {
  status.describe_error(NeedPassword)
  |> should.equal("user name okay need password")
}

pub fn description_login_need_account_test() {
  status.describe_error(LoginNeedAccount)
  |> should.equal("need account for login")
}

pub fn description_request_file_pending_test() {
  status.describe_error(RequestFilePending)
  |> should.equal("requested file action pending further information")
}

pub fn description_not_available_test() {
  status.describe_error(NotAvailable)
  |> should.equal("service not available closing control connection")
}

pub fn description_cannot_open_data_connection_test() {
  status.describe_error(CannotOpenDataConnection)
  |> should.equal("can't open data connection")
}

pub fn description_transfer_aborted_test() {
  status.describe_error(TransferAborted)
  |> should.equal("connection closed; transfer aborted")
}

pub fn description_invalid_credentials_test() {
  status.describe_error(InvalidCredentials)
  |> should.equal("invalid username or password")
}

pub fn description_host_unavailable_test() {
  status.describe_error(HostUnavailable)
  |> should.equal("requested host unavailable")
}

pub fn description_request_file_action_ignored_test() {
  status.describe_error(RequestFileActionIgnored)
  |> should.equal("requested file action not taken")
}

pub fn description_action_aborted_test() {
  status.describe_error(ActionAborted)
  |> should.equal("requested action aborted")
}

pub fn description_requested_action_not_taken_test() {
  status.describe_error(RequestedActionNotTaken)
  |> should.equal("requested action not taken")
}

pub fn description_bad_command_test() {
  status.describe_error(BadCommand)
  |> should.equal("syntax error command unrecognized")
}

pub fn description_bad_arguments_test() {
  status.describe_error(BadArguments)
  |> should.equal("syntax error in parameters or arguments")
}

pub fn description_not_implemented_test() {
  status.describe_error(NotImplemented)
  |> should.equal("comamnd not implemented")
}

pub fn description_bad_sequence_test() {
  status.describe_error(BadSequence)
  |> should.equal("bad sequence of commands")
}

pub fn description_not_implemented_parameter_test() {
  status.describe_error(NotImplementedParameter)
  |> should.equal("command not implemented for that parameter")
}

pub fn description_not_logged_in_test() {
  status.describe_error(NotLoggedIn)
  |> should.equal("user not logged in")
}

pub fn description_storing_need_account_test() {
  status.describe_error(StoringNeedAccount)
  |> should.equal("need account for storing files")
}

pub fn description_file_unavailable_test() {
  status.describe_error(FileUnavailable)
  |> should.equal("requested action not taken; file unavailable")
}

pub fn description_page_type_unknown_test() {
  status.describe_error(PageTypeUnknown)
  |> should.equal("requested action aborted; page type unknown")
}

pub fn description_exceeded_storage_test() {
  status.describe_error(ExceededStorage)
  |> should.equal("requested file action aborted; execeeded storage allocation")
}

pub fn description_bad_filename_test() {
  status.describe_error(BadFilename)
  |> should.equal("requested action not taken; file name not allowed")
}

pub fn description_unknown_test() {
  status.describe_error(Unknown)
  |> should.equal("unknown error code")
}
