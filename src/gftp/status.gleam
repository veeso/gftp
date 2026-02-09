//// This module exposes all the "standard" error codes defined in the File transfer protocol

/// The `Status` type represents the various status codes that can be returned by an FTP server.
/// Each variant corresponds to a specific status code defined in the FTP protocol,
/// categorized into different classes based on their meaning
/// (e.g., positive preliminary reply, positive completion reply, etc.).
pub type Status {
  // 1xx: Positive Preliminary Reply
  RestartMarker
  ReadyMinute
  AlreadyOpen
  AboutToSend
  // 2xx: Positive Completion Reply
  CommandOk
  CommandNotImplemented
  System
  Directory
  File
  Help
  Name
  Ready
  Closing
  DataConnectionOpen
  ClosingDataConnection
  PassiveMode
  LongPassiveMode
  ExtendedPassiveMode
  LoggedIn
  LoggedOut
  LogoutAck
  AuthOk
  RequestedFileActionOk
  PathCreated
  // 3xx: Positive intermediate Reply
  NeedPassword
  LoginNeedAccount
  RequestFilePending
  // 4xx: Transient Negative Completion Reply
  NotAvailable
  CannotOpenDataConnection
  TransferAborted
  InvalidCredentials
  HostUnavailable
  RequestFileActionIgnored
  ActionAborted
  RequestedActionNotTaken
  // 5xx: Permanent Negative Completion Reply
  BadCommand
  BadArguments
  NotImplemented
  BadSequence
  NotImplementedParameter
  NotLoggedIn
  StoringNeedAccount
  FileUnavailable
  PageTypeUnknown
  ExceededStorage
  BadFilename
  Unknown
}

/// Converts a status code to its descriptive string representation. This is useful for logging and debugging purposes.
pub fn describe_error(status: Status) -> String {
  case status {
    RestartMarker -> "restart marker reply"
    ReadyMinute -> "service ready in (n) minutes"
    AlreadyOpen -> "data connection already open transfer starting"
    AboutToSend -> "file status okay about to open data connection"
    CommandOk -> "command okay"
    CommandNotImplemented -> "command not implemented"
    System -> "system status or system help reply"
    Directory -> "directory status"
    File -> "file status"
    Help -> "help message"
    Name -> "NAME system type"
    Ready -> "service ready for new user"
    Closing -> "service closing control connection"
    DataConnectionOpen -> "data connection open; no transfer in progress"
    ClosingDataConnection -> "closing data connection"
    PassiveMode -> "entering passive mode"
    LongPassiveMode -> "entering long passive mode"
    ExtendedPassiveMode -> "entering extended passive mode"
    LoggedIn -> "user logged in proceed. Logged out if appropriate."
    LoggedOut -> "user logged out; service terminated"
    LogoutAck -> "logout command noted will complete when transfer done"
    AuthOk ->
      "specifies that the server accepts the authentication mechanism specified by the client"
    RequestedFileActionOk -> "requested file action okay"
    PathCreated -> "pathname created"
    NeedPassword -> "user name okay need password"
    LoginNeedAccount -> "need account for login"
    RequestFilePending -> "requested file action pending further information"
    NotAvailable -> "service not available closing control connection"
    CannotOpenDataConnection -> "can't open data connection"
    TransferAborted -> "connection closed; transfer aborted"
    InvalidCredentials -> "invalid username or password"
    HostUnavailable -> "requested host unavailable"
    RequestFileActionIgnored -> "requested file action not taken"
    ActionAborted -> "requested action aborted"
    RequestedActionNotTaken -> "requested action not taken"
    BadCommand -> "syntax error command unrecognized"
    BadArguments -> "syntax error in parameters or arguments"
    NotImplemented -> "comamnd not implemented"
    BadSequence -> "bad sequence of commands"
    NotImplementedParameter -> "command not implemented for that parameter"
    NotLoggedIn -> "user not logged in"
    StoringNeedAccount -> "need account for storing files"
    FileUnavailable -> "requested action not taken; file unavailable"
    PageTypeUnknown -> "requested action aborted; page type unknown"
    ExceededStorage ->
      "requested file action aborted; execeeded storage allocation"
    BadFilename -> "requested action not taken; file name not allowed"
    Unknown -> "unknown error code"
  }
}

/// Converts an integer status code to its corresponding `Status` variant. If the code is not recognized, it returns `Unknown`.
pub fn from_int(code: Int) -> Status {
  case code {
    110 -> RestartMarker
    120 -> ReadyMinute
    125 -> AlreadyOpen
    150 -> AboutToSend
    200 -> CommandOk
    202 -> CommandNotImplemented
    211 -> System
    212 -> Directory
    213 -> File
    214 -> Help
    215 -> Name
    220 -> Ready
    221 -> Closing
    225 -> DataConnectionOpen
    226 -> ClosingDataConnection
    227 -> PassiveMode
    228 -> LongPassiveMode
    229 -> ExtendedPassiveMode
    230 -> LoggedIn
    231 -> LoggedOut
    232 -> LogoutAck
    234 -> AuthOk
    250 -> RequestedFileActionOk
    257 -> PathCreated
    331 -> NeedPassword
    332 -> LoginNeedAccount
    350 -> RequestFilePending
    421 -> NotAvailable
    425 -> CannotOpenDataConnection
    426 -> TransferAborted
    430 -> InvalidCredentials
    434 -> HostUnavailable
    450 -> RequestFileActionIgnored
    451 -> ActionAborted
    452 -> RequestedActionNotTaken
    500 -> BadCommand
    501 -> BadArguments
    502 -> NotImplemented
    503 -> BadSequence
    504 -> NotImplementedParameter
    530 -> NotLoggedIn
    532 -> StoringNeedAccount
    550 -> FileUnavailable
    551 -> PageTypeUnknown
    552 -> ExceededStorage
    553 -> BadFilename
    _ -> Unknown
  }
}
