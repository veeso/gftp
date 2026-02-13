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

// --- from_int tests ---

pub fn from_int_restart_marker_test() {
  let assert RestartMarker = status.from_int(110)
}

pub fn from_int_ready_minute_test() {
  let assert ReadyMinute = status.from_int(120)
}

pub fn from_int_already_open_test() {
  let assert AlreadyOpen = status.from_int(125)
}

pub fn from_int_about_to_send_test() {
  let assert AboutToSend = status.from_int(150)
}

pub fn from_int_command_ok_test() {
  let assert CommandOk = status.from_int(200)
}

pub fn from_int_command_not_implemented_test() {
  let assert CommandNotImplemented = status.from_int(202)
}

pub fn from_int_system_test() {
  let assert System = status.from_int(211)
}

pub fn from_int_directory_test() {
  let assert Directory = status.from_int(212)
}

pub fn from_int_file_test() {
  let assert File = status.from_int(213)
}

pub fn from_int_help_test() {
  let assert Help = status.from_int(214)
}

pub fn from_int_name_test() {
  let assert Name = status.from_int(215)
}

pub fn from_int_ready_test() {
  let assert Ready = status.from_int(220)
}

pub fn from_int_closing_test() {
  let assert Closing = status.from_int(221)
}

pub fn from_int_data_connection_open_test() {
  let assert DataConnectionOpen = status.from_int(225)
}

pub fn from_int_closing_data_connection_test() {
  let assert ClosingDataConnection = status.from_int(226)
}

pub fn from_int_passive_mode_test() {
  let assert PassiveMode = status.from_int(227)
}

pub fn from_int_long_passive_mode_test() {
  let assert LongPassiveMode = status.from_int(228)
}

pub fn from_int_extended_passive_mode_test() {
  let assert ExtendedPassiveMode = status.from_int(229)
}

pub fn from_int_logged_in_test() {
  let assert LoggedIn = status.from_int(230)
}

pub fn from_int_logged_out_test() {
  let assert LoggedOut = status.from_int(231)
}

pub fn from_int_logout_ack_test() {
  let assert LogoutAck = status.from_int(232)
}

pub fn from_int_auth_ok_test() {
  let assert AuthOk = status.from_int(234)
}

pub fn from_int_requested_file_action_ok_test() {
  let assert RequestedFileActionOk = status.from_int(250)
}

pub fn from_int_path_created_test() {
  let assert PathCreated = status.from_int(257)
}

pub fn from_int_need_password_test() {
  let assert NeedPassword = status.from_int(331)
}

pub fn from_int_login_need_account_test() {
  let assert LoginNeedAccount = status.from_int(332)
}

pub fn from_int_request_file_pending_test() {
  let assert RequestFilePending = status.from_int(350)
}

pub fn from_int_not_available_test() {
  let assert NotAvailable = status.from_int(421)
}

pub fn from_int_cannot_open_data_connection_test() {
  let assert CannotOpenDataConnection = status.from_int(425)
}

pub fn from_int_transfer_aborted_test() {
  let assert TransferAborted = status.from_int(426)
}

pub fn from_int_invalid_credentials_test() {
  let assert InvalidCredentials = status.from_int(430)
}

pub fn from_int_host_unavailable_test() {
  let assert HostUnavailable = status.from_int(434)
}

pub fn from_int_request_file_action_ignored_test() {
  let assert RequestFileActionIgnored = status.from_int(450)
}

pub fn from_int_action_aborted_test() {
  let assert ActionAborted = status.from_int(451)
}

pub fn from_int_requested_action_not_taken_test() {
  let assert RequestedActionNotTaken = status.from_int(452)
}

pub fn from_int_bad_command_test() {
  let assert BadCommand = status.from_int(500)
}

pub fn from_int_bad_arguments_test() {
  let assert BadArguments = status.from_int(501)
}

pub fn from_int_not_implemented_test() {
  let assert NotImplemented = status.from_int(502)
}

pub fn from_int_bad_sequence_test() {
  let assert BadSequence = status.from_int(503)
}

pub fn from_int_not_implemented_parameter_test() {
  let assert NotImplementedParameter = status.from_int(504)
}

pub fn from_int_not_logged_in_test() {
  let assert NotLoggedIn = status.from_int(530)
}

pub fn from_int_storing_need_account_test() {
  let assert StoringNeedAccount = status.from_int(532)
}

pub fn from_int_file_unavailable_test() {
  let assert FileUnavailable = status.from_int(550)
}

pub fn from_int_page_type_unknown_test() {
  let assert PageTypeUnknown = status.from_int(551)
}

pub fn from_int_exceeded_storage_test() {
  let assert ExceededStorage = status.from_int(552)
}

pub fn from_int_bad_filename_test() {
  let assert BadFilename = status.from_int(553)
}

pub fn from_int_unknown_test() {
  let assert Unknown(999) = status.from_int(999)
}

// --- description tests ---

pub fn description_restart_marker_test() {
  let assert "restart marker reply" = status.describe(RestartMarker)
}

pub fn description_ready_minute_test() {
  let assert "service ready in (n) minutes" = status.describe(ReadyMinute)
}

pub fn description_already_open_test() {
  let assert "data connection already open transfer starting" =
    status.describe(AlreadyOpen)
}

pub fn description_about_to_send_test() {
  let assert "file status okay about to open data connection" =
    status.describe(AboutToSend)
}

pub fn description_command_ok_test() {
  let assert "command okay" = status.describe(CommandOk)
}

pub fn description_command_not_implemented_test() {
  let assert "command not implemented" = status.describe(CommandNotImplemented)
}

pub fn description_system_test() {
  let assert "system status or system help reply" = status.describe(System)
}

pub fn description_directory_test() {
  let assert "directory status" = status.describe(Directory)
}

pub fn description_file_test() {
  let assert "file status" = status.describe(File)
}

pub fn description_help_test() {
  let assert "help message" = status.describe(Help)
}

pub fn description_name_test() {
  let assert "NAME system type" = status.describe(Name)
}

pub fn description_ready_test() {
  let assert "service ready for new user" = status.describe(Ready)
}

pub fn description_closing_test() {
  let assert "service closing control connection" = status.describe(Closing)
}

pub fn description_data_connection_open_test() {
  let assert "data connection open; no transfer in progress" =
    status.describe(DataConnectionOpen)
}

pub fn description_closing_data_connection_test() {
  let assert "closing data connection" = status.describe(ClosingDataConnection)
}

pub fn description_passive_mode_test() {
  let assert "entering passive mode" = status.describe(PassiveMode)
}

pub fn description_long_passive_mode_test() {
  let assert "entering long passive mode" = status.describe(LongPassiveMode)
}

pub fn description_extended_passive_mode_test() {
  let assert "entering extended passive mode" =
    status.describe(ExtendedPassiveMode)
}

pub fn description_logged_in_test() {
  let assert "user logged in proceed. Logged out if appropriate." =
    status.describe(LoggedIn)
}

pub fn description_logged_out_test() {
  let assert "user logged out; service terminated" = status.describe(LoggedOut)
}

pub fn description_logout_ack_test() {
  let assert "logout command noted will complete when transfer done" =
    status.describe(LogoutAck)
}

pub fn description_auth_ok_test() {
  let assert "specifies that the server accepts the authentication mechanism specified by the client" =
    status.describe(AuthOk)
}

pub fn description_requested_file_action_ok_test() {
  let assert "requested file action okay" =
    status.describe(RequestedFileActionOk)
}

pub fn description_path_created_test() {
  let assert "pathname created" = status.describe(PathCreated)
}

pub fn description_need_password_test() {
  let assert "user name okay need password" = status.describe(NeedPassword)
}

pub fn description_login_need_account_test() {
  let assert "need account for login" = status.describe(LoginNeedAccount)
}

pub fn description_request_file_pending_test() {
  let assert "requested file action pending further information" =
    status.describe(RequestFilePending)
}

pub fn description_not_available_test() {
  let assert "service not available closing control connection" =
    status.describe(NotAvailable)
}

pub fn description_cannot_open_data_connection_test() {
  let assert "can't open data connection" =
    status.describe(CannotOpenDataConnection)
}

pub fn description_transfer_aborted_test() {
  let assert "connection closed; transfer aborted" =
    status.describe(TransferAborted)
}

pub fn description_invalid_credentials_test() {
  let assert "invalid username or password" =
    status.describe(InvalidCredentials)
}

pub fn description_host_unavailable_test() {
  let assert "requested host unavailable" = status.describe(HostUnavailable)
}

pub fn description_request_file_action_ignored_test() {
  let assert "requested file action not taken" =
    status.describe(RequestFileActionIgnored)
}

pub fn description_action_aborted_test() {
  let assert "requested action aborted" = status.describe(ActionAborted)
}

pub fn description_requested_action_not_taken_test() {
  let assert "requested action not taken" =
    status.describe(RequestedActionNotTaken)
}

pub fn description_bad_command_test() {
  let assert "syntax error command unrecognized" = status.describe(BadCommand)
}

pub fn description_bad_arguments_test() {
  let assert "syntax error in parameters or arguments" =
    status.describe(BadArguments)
}

pub fn description_not_implemented_test() {
  let assert "command not implemented" = status.describe(NotImplemented)
}

pub fn description_bad_sequence_test() {
  let assert "bad sequence of commands" = status.describe(BadSequence)
}

pub fn description_not_implemented_parameter_test() {
  let assert "command not implemented for that parameter" =
    status.describe(NotImplementedParameter)
}

pub fn description_not_logged_in_test() {
  let assert "user not logged in" = status.describe(NotLoggedIn)
}

pub fn description_storing_need_account_test() {
  let assert "need account for storing files" =
    status.describe(StoringNeedAccount)
}

pub fn description_file_unavailable_test() {
  let assert "requested action not taken; file unavailable" =
    status.describe(FileUnavailable)
}

pub fn description_page_type_unknown_test() {
  let assert "requested action aborted; page type unknown" =
    status.describe(PageTypeUnknown)
}

pub fn description_exceeded_storage_test() {
  let assert "requested file action aborted; exceeded storage allocation" =
    status.describe(ExceededStorage)
}

pub fn description_bad_filename_test() {
  let assert "requested action not taken; file name not allowed" =
    status.describe(BadFilename)
}

pub fn description_unknown_test() {
  let assert "unknown error code" = status.describe(Unknown(999))
}

// --- to_int tests ---

pub fn to_int_known_status_test() {
  let assert 200 = status.to_int(CommandOk)
}

pub fn to_int_unknown_preserves_code_test() {
  let assert 999 = status.to_int(Unknown(999))
}

pub fn to_int_roundtrip_test() {
  let assert 226 = status.to_int(status.from_int(226))
}

pub fn to_int_unknown_roundtrip_test() {
  let assert 777 = status.to_int(status.from_int(777))
}
