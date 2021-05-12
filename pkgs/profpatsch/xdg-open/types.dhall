let Mime = List Text

let
    -- TODO use library like with shell commands
    Executable =
      Text

let Arg = < String : Text | Variable : Text >

let CommandTemplate =
      λ(templates : Type) → { exe : Executable, args : templates → List Arg }

let Command = CommandTemplate Arg

let Special =
      { open-in-editor : Command
      , open-in-browser : Command
      , fetch-http-url-mime : Command
      , compose-mail-to : Command
      , exec-in-terminal-emulator : ∀(args : Command) → Command
      , dmenu-list-binaries-and-exec : Command
      , notify : ∀(message : Text) → Command
      }

let
    -- Handler of an uri glob. Mime maps the uri to a file handler. Transparent is a command which, when run, returns a mimetype of the file.
    UriGlobHandler =
      < Transparent : Command | Mime : Mime >

let UriMimeGlob =
      { desc : Text
      , -- less specific than glob, used by firefox to refer to the schema
        schema-prefix : List Text
      , -- schema shell glob to check whether a link corresponds to the schema
        glob : List Text
      , handler : UriGlobHandler
      }

let
    -- describes the command `cmd` to run for the matched mime type `mime`
    MimeMatch =
      { mime : Mime, cmd : Command }

in  { Mime
    , Executable
    , Arg
    , CommandTemplate
    , Command
    , Special
    , UriGlobHandler
    , UriMimeGlob
    , MimeMatch
    }
