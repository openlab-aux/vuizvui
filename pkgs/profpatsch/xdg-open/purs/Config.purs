module Config where

import XdgOpen

type Special cmd =
  { openInBrowser :: cmd
  , fetchHttpUrlMime :: cmd
  , composeMailTo :: cmd
  , execInTerminalEmulator :: cmd -> cmd
  , dmenuListBinariesAndExec :: cmd
  , notify :: String → cmd
  }

type DrvBinary = { package :: String, binary :: String }
type Pkgs = { pkg :: DrvBinary -> Executable, pkgOnDemand :: DrvBinary -> Executable }

wrapCommand :: Command -> Command -> Command
wrapCommand wrapper cmd =
  { exe: wrapper.exe
  , args: \(template :: Arg) ->
      wrapper.args
        template ++ [ ArgString cmd.exe ] ++ cmd.args template
  }

oneArg :: Executable -> Command
oneArg exe = { exe, args: \(file :: Arg) -> [ file ] }

mime :: Pkgs -> Special Command -> Config
mime pkgs special = do
  let
    pkgSame :: String -> Executable
    pkgSame name = pkgs.pkg { package: name, binary: name }

    pkgSameOnDemand :: String -> Executable
    pkgSameOnDemand name = pkgs.pkgOnDemand { package: name, binary: name }

    pkgFromPath p = p

  let
    m =
      { text:
          { html: { mime: [ "text", "html" ], cmd: special.openInBrowser }
          , gemini:
              { mime: [ "text", "gemini" ]
              , cmd: oneArg (pkgSameOnDemand "lagrange")
              }
          , gopher:
              { mime: [ "text", "gopher" ]
              , cmd: oneArg (pkgSameOnDemand "lagrange")
              }
          , xml:
              { mime: [ "text", "xml" ], cmd: special.openInBrowser }
          , csv:
              { mime: [ "text", "csv" ]
              , cmd: oneArg (pkgSameOnDemand "libreoffice")
              }
          , any:
              { mime: [ "text", "*" ],
                cmd: oneArg (pkgSameOnDemand "micro") }

          }
      , mailAddress:
          { mime: [ "special", "mailaddress" ]
          , cmd: special.composeMailTo
          }
      , torrent:
          { mime: [ "application", "x-bittorrent" ]
          , cmd: special.notify "No xdg-open handler for the torrent"
          }
      , irc:
          { mime: [ "x-scheme-handler", "irc" ]
          , cmd: special.notify "No xdg-open handler for the irc link"
          }
      , file:
          { mime: [ "x-scheme-handler", "file" ]
          , cmd:
              special.notify
                "No xdg-open handler for the x-scheme-handler/file"
          }
      , image:
          { gif:
              { mime: [ "image", "gif" ], cmd: special.openInBrowser }
          , svg:
              { mime: [ "image", "svg+xml" ]
              , cmd: oneArg (pkgFromPath "inkscape")
              }
          , any:
              { mime: [ "image", "*" ], cmd: oneArg (pkgSame "imv") }
          }
      , pdf:
          { mime: [ "application", "pdf" ]
          , cmd: oneArg (pkgSame "zathura")
          }
      , pgpKey:
          { mime: [ "application", "pgp-keys" ]
          , cmd:
              { exe: pkgs.pkg { package: "gnupg", binary: "gpg" }
              , args:
                  \(file :: Arg) ->
                    [ ArgString "--import"
                    , ArgString "--import-options"
                    , ArgString "show-only"
                    , file
                    ]
              }
          }
      , directory:
          { mime: [ "inode", "directory" ]
          , cmd:
              special.execInTerminalEmulator
                (oneArg (pkgSame "ranger"))
          }
      , opendocumentAny:
          { mime: [ "application/vnd.oasis.opendocument.*" ]
          , cmd: oneArg (pkgSameOnDemand "libreoffice")
          }
      , openxmlformatsAny:
          { mime: [ "application/vnd.openxmlformats-officedocument.*" ]
          , cmd: oneArg (pkgSameOnDemand "libreoffice")
          }
      , msword:
          { mime: [ "application/msword" ]
          , cmd: oneArg (pkgSameOnDemand "libreoffice")
          }
      , any:
          { mime: [ "*" ], cmd: special.dmenuListBinariesAndExec }
      }
  { orderedMimeMatchers:
      [ m.text.html
      , m.text.gemini
      , m.text.gopher
      , m.text.xml
      , m.text.csv
      , m.text.any
      , m.mailAddress
      , m.torrent
      , m.irc
      , m.file
      , m.image.gif
      , m.image.svg
      , m.image.any
      , m.pdf
      , m.opendocumentAny
      , m.openxmlformatsAny
      , m.pgpKey
      , m.directory
      , m.any
      ]
  , uriMimeGlobs:
      [ { desc: "http link"
        , glob: [ "http://*", "https://*" ]
        , schemaPrefix: [ "http", "https" ]
        , handler: m.text.html
        }
      , { desc: "gemini link"
        , glob: [ "gemini://*" ]
        , schemaPrefix: [ "gemini" ]
        , handler: m.text.gemini
        }
      , { desc: "gemini link"
        , glob: [ "gopher://*", "gophers://*" ]
        , schemaPrefix: [ "gopher", "gophers" ]
        , handler: m.text.gopher
        }
      , { glob: [ "mailto:*" ]
        , desc: "mail address"
        , schemaPrefix: [ "mailto" ]
        , handler: m.mailAddress
        }
      , { glob: [ "magnet:*" ]
        , desc: "bittorrent magnet link"
        , schemaPrefix: [ "magnet" ]
        , handler: m.torrent
        }
      , { desc: "irc channel"
        , glob: [ "irc:*", "ircs:*" ]
        , schemaPrefix: [ "irc", "ircs" ]
        , handler: m.irc
        }
      ] :: Array UriMimeGlob

  }

