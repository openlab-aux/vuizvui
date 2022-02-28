let types = ./types.dhall

let Executable = types.Executable

let Special = types.Special

let Command = types.Command

let Arg = types.Arg

let Mime = types.Mime

let UriMimeGlob = types.UriMimeGlob

let MimeMatch = types.MimeMatch

in  λ(pkgs : { package : Text, binary : Text } → Executable) →
    λ(pkgsOnDemand : { package : Text, binary : Text } → Executable) →
    λ(special : Special) →
      let mime =
            let pkgSame =
                  λ(packageAndBinaryName : Text) →
                    pkgs
                      { package = packageAndBinaryName
                      , binary = packageAndBinaryName
                      }

            let pkgSameOnDemand =
                  λ(packageAndBinaryName : Text) →
                    pkgsOnDemand
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

            let
                -- An executable that takes as its one argument the file to run
                oneArg =
                  λ(exe : Executable) → { exe, args = λ(file : Arg) → [ file ] }

            in  { text =
                  { html =
                    { mime = [ "text", "html" ], cmd = special.open-in-browser }
                  , gemini =
                    { mime = [ "text", "gemini" ]
                    , cmd = oneArg (pkgSame "lagrange")
                    }
                  , gopher =
                    { mime = [ "text", "gopher" ]
                    , cmd = oneArg (pkgSame "lagrange")
                    }
                  , xml =
                    { mime = [ "text", "xml" ], cmd = special.open-in-browser }
                  , ical =
                    { mime = [ "text", "calendar" ]
                    , cmd = special.add-to-calendar
                    }
                  , csv =
                    { mime = [ "text", "csv" ]
                    , cmd = oneArg (pkgSameOnDemand "libreoffice")
                    }
                  , any =
                    { mime = [ "text", "any" ], cmd = special.open-in-editor }
                  }
                , mail-address =
                  { mime = [ "special", "mailaddress" ]
                  , cmd = special.compose-mail-to
                  }
                , torrent =
                  { mime = [ "application", "x-bittorrent" ]
                  , cmd = special.notify "No xdg-open handler for the torrent"
                  }
                , irc =
                  { mime = [ "x-scheme-handler", "irc" ]
                  , cmd = special.notify "No xdg-open handler for the irc link"
                  }
                , file =
                  { mime = [ "x-scheme-handler", "file" ]
                  , cmd =
                      special.notify
                        "No xdg-open handler for the x-scheme-handler/file"
                  }
                , image =
                  { gif =
                    { mime = [ "image", "gif" ], cmd = special.open-in-browser }
                  , svg =
                    { mime = [ "image", "svg+xml" ]
                    , cmd = oneArg (pkgSame "inkscape")
                    }
                  , any =
                    { mime = [ "image", "*" ], cmd = oneArg (pkgSame "imv") }
                  }
                , pdf =
                  { mime = [ "application", "pdf" ]
                  , cmd = oneArg (pkgSame "zathura")
                  }
                , pgp-key =
                  { mime = [ "application", "pgp-keys" ]
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
                , directory =
                  { mime = [ "inode", "directory" ]
                  , cmd =
                      special.exec-in-terminal-emulator
                        (oneArg (pkgSame "ranger"))
                  }
                , opendocument-any =
                  { mime = [ "application/vnd.oasis.opendocument.*" ]
                  , cmd = oneArg (pkgSameOnDemand "libreoffice")
                  }
                , openxmlformats-any =
                  { mime = [ "application/vnd.openxmlformats-officedocument.*" ]
                  , cmd = oneArg (pkgSameOnDemand "libreoffice")
                  }
                , any =
                  { mime = [ "*" ], cmd = special.dmenu-list-binaries-and-exec }
                }

      let orderedMimeMatchers =
            [ mime.text.html
            , mime.text.gemini
            , mime.text.gopher
            , mime.text.xml
            , mime.text.ical
            , mime.text.csv
            , mime.text.any
            , mime.mail-address
            , mime.torrent
            , mime.irc
            , mime.file
            , mime.image.gif
            , mime.image.svg
            , mime.image.any
            , mime.pdf
            , mime.opendocument-any
            , mime.openxmlformats-any
            , mime.pgp-key
            , mime.directory
            , mime.any
            ]

      let uriMimeGlobs
          : List UriMimeGlob
          = [ { desc = "http link"
              , glob = [ "http://*", "https://*" ]
              , schema-prefix = [ "http", "https" ]
              , handler = mime.text.html
              }
            , { desc = "gemini link"
              , glob = [ "gemini://*" ]
              , schema-prefix = [ "gemini" ]
              , handler = mime.text.gemini
              }
            , { desc = "gemini link"
              , glob = [ "gopher://*", "gophers://*" ]
              , schema-prefix = [ "gopher", "gophers" ]
              , handler = mime.text.gopher
              }
            , { glob = [ "mailto:*" ]
              , desc = "mail address"
              , schema-prefix = [ "mailto" ]
              , handler = mime.mail-address
              }
            , { glob = [ "magnet:*" ]
              , desc = "bittorrent magnet link"
              , schema-prefix = [ "magnet" ]
              , handler = mime.torrent
              }
            , { desc = "irc channel"
              , glob = [ "irc:*", "ircs:*" ]
              , schema-prefix = [ "irc", "ircs" ]
              , handler = mime.irc
              }
            ]

      in  { uriMimeGlobs
          , UriMimeGlob
          , orderedMimeMatchers
          , Executable
          , Command
          , MimeMatch
          , Special
          , Mime
          , Arg
          }
