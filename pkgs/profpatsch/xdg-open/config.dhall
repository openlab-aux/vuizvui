let Mime = List Text

let
    -- TODO use library like with shell commands
    Executable =
      Text

let Arg = < String : Text | Variable : Text >

let CommandTemplate =
      λ(templates : Type) → { exe : Executable, args : templates → List Arg }

let Command = CommandTemplate Text

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

let uriMimeGlobs =
      [ { desc = "http link"
        , glob = [ "http://*", "https://*" ]
        , mime = mime.text.html
        }
      , { glob = [ "mailto:*" ]
        , desc = "mail address"
        , mime = mime.mail-address
        }
      , { glob = [ "magnet:*" ]
        , desc = "bittorrent magnet link"
        , mime = mime.torrent
        }
      , { desc = "irc channel", glob = [ "irc:*" ], mime = mime.irc }
      , { desc = "local file", glob = [ "file://*" ], mime = mime.file }
      ]

let MimeMatch = { match : Mime, cmd : Command }

let Special =
      { open-in-editor : Command
      , open-in-browser : Command
      , compose-mail-to : Command
      , exec-in-terminal-emulator : ∀(args : Command) → Command
      , dmenu-list-binaries-and-exec : Command
      }

let mimeMatcher =
        λ(pkgs : { package : Text, binary : Text } → Executable)
      → λ(special : Special)
      → let pkgSame =
                λ(packageAndBinaryName : Text)
              → pkgs
                  { package = packageAndBinaryName
                  , binary = packageAndBinaryName
                  }

        let oneArg =
                λ(exe : Executable)
              → { exe = exe, args = λ(file : Text) → [ Arg.Variable file ] }

        let m =
              λ(match : Mime) → λ(cmd : Command) → { match = match, cmd = cmd }

        in    [ { match = mime.mail-address, cmd = special.compose-mail-to }
              , { match = mime.text.html, cmd = special.open-in-browser }
              , { match = mime.text.xml, cmd = special.open-in-browser }
              , { match = mime.text.any, cmd = special.open-in-editor }
              , { match = mime.image.gif, cmd = special.open-in-browser }
              , { match = mime.image.svg, cmd = oneArg (pkgSame "inkscape") }
              , { match = mime.image.any, cmd = oneArg (pkgSame "feh") }
              , { match = mime.pdf, cmd = oneArg (pkgSame "zathura") }
              , { match = mime.pgp-key
                , cmd =
                  { exe = pkgs { package = "gnupg", binary = "gpg" }
                  , args =
                        λ(file : Text)
                      → [ Arg.String "--import"
                        , Arg.String "--import-options"
                        , Arg.String "show-only"
                        , Arg.Variable file
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

in  { mimeMatcher = mimeMatcher
    , uriMimeGlobs = uriMimeGlobs
    , Executable = Executable
    , Command = Command
    , MimeMatch = MimeMatch
    , Special = Special
    , Mime = Mime
    , Arg = Arg
    }
