let types = ./types.dhall

let Executable = types.Executable

let Special = types.Special

let Command = types.Command

let Arg = types.Arg

let Mime = types.Mime

let UriGlobHandler = types.UriGlobHandler

let UriMimeGlob = types.UriMimeGlob

let MimeMatch = types.MimeMatch

in  λ(pkgs : { package : Text, binary : Text } → Executable) →
    λ(special : Special) →
      let mime =
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

            let
                -- An executable that takes as its one argument the file to run
                oneArg =
                  λ(exe : Executable) → { exe, args = λ(file : Arg) → [ file ] }

            in  { text =
                  { html =
                    { mime = [ "text", "html" ], cmd = special.compose-mail-to }
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
                , any =
                  { mime = [ "*" ], cmd = special.dmenu-list-binaries-and-exec }
                }

      let matchOrder =
            [ mime.text.html
            , mime.text.gemini
            , mime.text.gopher
            , mime.text.xml
            , mime.text.any
            , mime.mail-address
            , mime.torrent
            , mime.irc
            , mime.file
            , mime.image.gif
            , mime.image.svg
            , mime.image.any
            , mime.pdf
            , mime.pgp-key
            , mime.directory
            , mime.any
            ]

      let uriMimeGlobs
          : List UriMimeGlob
          = [ { desc = "http link"
              , glob = [ "http://*", "https://*" ]
              , schema-prefix = [ "http", "https" ]
              , handler =
                  let TODO =
                        UriGlobHandler.Transparent special.fetch-http-url-mime

                  in  UriGlobHandler.Mime mime.text.html.mime
              }
            , { desc = "gemini link"
              , glob = [ "gemini://*" ]
              , schema-prefix = [ "gemini" ]
              , handler = UriGlobHandler.Mime mime.text.gemini.mime
              }
            , { desc = "gemini link"
              , glob = [ "gopher://*", "gophers://*" ]
              , schema-prefix = [ "gopher", "gophers" ]
              , handler = UriGlobHandler.Mime mime.text.gopher.mime
              }
            , { glob = [ "mailto:*" ]
              , desc = "mail address"
              , schema-prefix = [ "mailto" ]
              , handler = UriGlobHandler.Mime mime.mail-address.mime
              }
            , { glob = [ "magnet:*" ]
              , desc = "bittorrent magnet link"
              , schema-prefix = [ "magnet" ]
              , handler = UriGlobHandler.Mime mime.torrent.mime
              }
            , { desc = "irc channel"
              , glob = [ "irc:*", "ircs:*" ]
              , schema-prefix = [ "irc", "ircs" ]
              , handler = UriGlobHandler.Mime mime.irc.mime
              }
            , { desc = "local file"
              , glob = [ "file://*" ]
              , schema-prefix = [ "file" ]
              , handler = UriGlobHandler.Mime mime.file.mime
              }
            ]

      let mimeMatcher =
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

            in    [ { match = mime.mail-address.mime
                    , cmd = special.compose-mail-to
                    }
                  , { match = mime.text.html.mime
                    , cmd = special.open-in-browser
                    }
                  , { match = mime.text.gemini.mime
                    , cmd = oneArg (pkgSame "lagrange")
                    }
                  , { match = mime.text.gopher.mime
                    , cmd = oneArg (pkgSame "lagrange")
                    }
                  , { match = mime.text.xml.mime
                    , cmd = special.open-in-browser
                    }
                  , { match = mime.text.any.mime, cmd = special.open-in-editor }
                  , { match = mime.image.gif.mime
                    , cmd = special.open-in-browser
                    }
                  , { match = mime.image.svg.mime
                    , cmd = oneArg (pkgSame "inkscape")
                    }
                  , { match = mime.image.any.mime
                    , cmd = oneArg (pkgSame "imv")
                    }
                  , { match = mime.pdf.mime, cmd = oneArg (pkgSame "zathura") }
                  , { match = mime.pgp-key.mime
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
                  , { match = mime.directory.mime
                    , cmd =
                        special.exec-in-terminal-emulator
                          (oneArg (pkgSame "ranger"))
                    }
                  , { match = mime.any.mime
                    , cmd = special.dmenu-list-binaries-and-exec
                    }
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
