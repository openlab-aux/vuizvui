let Mime = List Text

let
    -- TODO use library like with shell commands
    Executable =
      Text

let Arg = < String : Text | Variable : Text >

let CommandTemplate =
      λ(templates : Type) → { exe : Executable, args : templates → List Arg }

let Command = CommandTemplate Arg

let mime =
      { text =
        { html = [ "text", "html" ]
        , xml = [ "text", "xml" ]
        , any = [ "text", "*" ]
        }
      , mail-address = [ "special", "mailaddress" ]
      , torrent = [ "application", "x-bittorrent" ]
      , irc = [ "x-scheme-handler", "irc" ]
      , file = [ "x-scheme-handler", "file" ]
      , image =
        { gif = [ "image", "gif" ]
        , svg = [ "image", "svg+xml" ]
        , any = [ "image", "*" ]
        }
      , pdf = [ "application", "pdf" ]
      , pgp-key = [ "application", "pgp-keys" ]
      , directory = [ "inode", "directory" ]
      , any = [ "*" ]
      }

let renderMime = λ(m : Mime) → 32

let
    -- Handler of an uri glob. Mime maps the uri to a file handler. Transparent is a command which, when run, returns a mimetype of the file.
    UriGlobHandler =
      < Transparent : Command | Mime : Mime >

let UriMimeGlob = { desc : Text, glob : List Text, handler : UriGlobHandler }

let Special =
      { open-in-editor : Command
      , open-in-browser : Command
      , fetch-http-url-mime : Command
      , compose-mail-to : Command
      , exec-in-terminal-emulator : ∀(args : Command) → Command
      , dmenu-list-binaries-and-exec : Command
      }

let uriMimeGlobs
    : Special → List UriMimeGlob
    = λ(special : Special) →
        [ { desc = "http link"
          , glob = [ "http://*", "https://*" ]
          , handler =
              let TODO = UriGlobHandler.Transparent special.fetch-http-url-mime

              in  UriGlobHandler.Mime mime.text.html
          }
        , { glob = [ "mailto:*" ]
          , desc = "mail address"
          , handler = UriGlobHandler.Mime mime.mail-address
          }
        , { glob = [ "magnet:*" ]
          , desc = "bittorrent magnet link"
          , handler = UriGlobHandler.Mime mime.torrent
          }
        , { desc = "irc channel"
          , glob = [ "irc:*" ]
          , handler = UriGlobHandler.Mime mime.irc
          }
        , { desc = "local file"
          , glob = [ "file://*" ]
          , handler = UriGlobHandler.Mime mime.file
          }
        ]

let MimeMatch = { match : Mime, cmd : Command }

let mimeMatcher =
      λ(pkgs : { package : Text, binary : Text } → Executable) →
      λ(special : Special) →
        let pkgSame =
              λ(packageAndBinaryName : Text) →
                pkgs
                  { package = packageAndBinaryName
                  , binary = packageAndBinaryName
                  }

        let wrapCommand =
              λ(wrapper : Command) →
              λ(cmd : Command) →
                { exe = wrapper.exe
                , args =
                    λ(template : Arg) →
                        wrapper.args template
                      # [ Arg.String cmd.exe ]
                      # cmd.args template
                }

        let oneArg =
              λ(exe : Executable) → { exe, args = λ(file : Arg) → [ file ] }

        let m = λ(match : Mime) → λ(cmd : Command) → { match, cmd }

        in    [ { match = mime.mail-address, cmd = special.compose-mail-to }
              , { match = mime.text.html, cmd = special.open-in-browser }
              , { match = mime.text.xml, cmd = special.open-in-browser }
              , { match = mime.text.any, cmd = special.open-in-editor }
              , { match = mime.image.gif, cmd = special.open-in-browser }
              , { match = mime.image.svg, cmd = oneArg (pkgSame "inkscape") }
              , { match = mime.image.any, cmd = oneArg (pkgSame "imv") }
              , { match = mime.pdf, cmd = oneArg (pkgSame "zathura") }
              , { match = mime.pgp-key
                , cmd =
                  { exe = pkgs { package = "gnupg", binary = "gpg" }
                  , args =
                      λ(file : Arg) →
                        [ Arg.String "--import"
                        , Arg.String "--import-options"
                        , Arg.String "show-only"
                        , file
                        ]
                  }
                }
              , { match = mime.directory
                , cmd =
                    special.exec-in-terminal-emulator
                      (oneArg (pkgSame "ranger"))
                }
              , { match = mime.any, cmd = special.dmenu-list-binaries-and-exec }
              ]
            : List MimeMatch

in  { mimeMatcher
    , uriMimeGlobs
    , UriMimeGlob
    , Executable
    , Command
    , UriGlobHandler
    , MimeMatch
    , Special
    , Mime
    , Arg
    }
