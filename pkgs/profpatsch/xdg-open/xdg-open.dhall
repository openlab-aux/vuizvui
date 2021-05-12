let Text/concatSep = ./imports/Prelude/Text/concatSep

let Text/concatMap = ./imports/Prelude/Text/concatMap

let List/concatMap = ./imports/Prelude/List/concatMap

let List/map = ./imports/Prelude/List/map

let
    -- TODO use library like with shell commands
    Executable =
      Text

let types = ./types.dhall

let renderMime = Text/concatSep "/"

let
    -- Escape the given shell command, at least the String arguments of it.
    -- Passes `$file` as variable argument.
    -- The final shell command is executed into.
    shellEscapeExecCommand =
      λ(shellEscape : Text → Text) →
      λ(file : Text) →
      λ(cmd : types.Command) →
          Text/concatSep
            " "
            (   [ "exec", shellEscape cmd.exe ]
              # List/map
                  types.Arg
                  Text
                  ( λ(arg : types.Arg) →
                      merge
                        { String = λ(t : Text) → shellEscape t
                        , Variable = λ(t : Text) → t
                        }
                        arg
                  )
                  (cmd.args (types.Arg.Variable file))
            )
        : Text

let repeatText =
      λ(t : Text) →
      λ(n : Natural) →
        Natural/fold n Text (λ(t2 : Text) → t ++ t2) ""

let Lines = { indent : Natural, lines : List Text }

let prettyLines =
      λ(lines : Lines) →
        Text/concatMap
          Text
          (λ(line : Text) → repeatText " " lines.indent ++ line ++ "\n")
          lines.lines

let xdg-open =
      let mimeMatcherCase =
            λ(shellEscape2 : Text → Text) →
            λ(file2 : Text) →
            λ(m : types.MimeMatch) →
              [ "${renderMime m.mime})"
              , "${shellEscapeExecCommand shellEscape2 file2 m.cmd}"
              , ";;"
              ]

      let mimeGlobCase =
            λ(shellEscape2 : Text → Text) →
            λ(file2 : Text) →
            λ(g : types.UriMimeGlob) →
                List/concatMap
                  Text
                  Text
                  ( λ(match : Text) →
                      merge
                        { Mime =
                            λ(mime : types.MimeMatch) →
                              [ "${match})"
                              , shellEscapeExecCommand
                                  shellEscape2
                                  file2
                                  mime.cmd
                              , ";;"
                              ]
                        , Transparent =
                            λ(cmd : types.Command) →
                              [ "${match})"
                              , "mime=\"\$(${shellEscapeExecCommand
                                               shellEscape2
                                               file2
                                               cmd})\""
                              , ";;"
                              ]
                        }
                        g.handler
                  )
                  g.glob
              : List Text

      in  λ(bins : { get-mime-type : Executable }) →
          λ(write-dash : Text → Text → Executable) →
          λ(shellEscape : Text → Text) →
          λ(pkgs : { package : Text, binary : Text } → Executable) →
          λ(special : types.Special) →
            let config = ./config.dhall pkgs special

            in  write-dash
                  "xdg-open"
                  (     ''
                        # TODO: --dry-run to display what would be opened and why

                        # partially taken from
                        # https://github.com/march-linux/mimi/blob/master/xdg-open

                        set -e
                        file="$1"
                        mime=

                        # match on protocols
                        # if you want to match files reliably, start with file://
                        case "$file" in
                        ${prettyLines
                            { indent = 2
                            , lines =
                                List/concatMap
                                  types.UriMimeGlob
                                  Text
                                  (mimeGlobCase shellEscape "\"\$file\"")
                                  config.uriMimeGlobs
                            }}
                          *)
                            # it’s a file

                            # strip possible protocol
                            file=''
                    ++  "\$"
                    ++  ''
                        {file#file://}
                            mime=$(file -E --brief --mime-type "$file") \
                            || (echo "$mime" 1>&2; exit 1)
                            # ^ echo the error message of file
                            ;;
                        esac

                        case "$mime" in
                        ${prettyLines
                            { indent = 2
                            , lines =
                                List/concatMap
                                  types.MimeMatch
                                  Text
                                  (mimeMatcherCase shellEscape "\"\$file\"")
                                  config.orderedMimeMatchers
                            }}
                        esac
                        ''
                  )

in  xdg-open
