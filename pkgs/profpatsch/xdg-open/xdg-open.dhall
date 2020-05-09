let Text/concatSep = ./imports/Prelude/Text/concatSep

let Text/concatMap = ./imports/Prelude/Text/concatMap

let List/concatMap = ./imports/Prelude/List/concatMap

let List/map = ./imports/Prelude/List/map

let
    -- TODO use library like with shell commands
    Executable =
      Text

let config = ./config.dhall

let foo =
      { match = [ "image", "png" ]
      , cmd = λ(_ : Text) → { exe = "echo", args = [ "foo" ] }
      }

let renderMime = Text/concatSep "/"

let shellEscapeCommand =
        λ(shellEscape : Text → Text)
      → λ(file : Text)
      → λ(cmd : config.Command)
      →   Text/concatSep
            " "
            (   [ shellEscape cmd.exe ]
              # List/map
                  config.Arg
                  Text
                  (   λ(arg : config.Arg)
                    → merge
                        { String = λ(t : Text) → shellEscape t
                        , Variable = λ(t : Text) → t
                        }
                        arg
                  )
                  (cmd.args file)
            )
        : Text

let repeatText =
        λ(t : Text)
      → λ(n : Natural)
      → Natural/fold n Text (λ(t2 : Text) → t ++ t2) ""

let Lines = { indent : Natural, lines : List Text }

let prettyLines =
        λ(lines : Lines)
      → Text/concatMap
          Text
          (λ(line : Text) → repeatText " " lines.indent ++ line ++ "\n")
          lines.lines

let xdg-open =
      let case =
              λ(shellEscape2 : Text → Text)
            → λ(file2 : Text)
            → λ(m : config.MimeMatch)
            → [ "${renderMime m.match})"
              , "${shellEscapeCommand shellEscape2 file2 m.cmd}"
              , ";;"
              ]

      in    λ(bins : { get-mime-type : Executable })
          → λ(write-dash : Text → Text → Executable)
          → λ(shellEscape : Text → Text)
          → λ(pkgs : { package : Text, binary : Text } → Executable)
          → λ(special : config.Special)
          → write-dash
              "xdg-open"
              ''
              file="$1"
              mime=$(${bins.get-mime-type} "$file")

              case "$mime" in
              ${prettyLines
                  { indent = 2
                  , lines =
                      List/concatMap
                        config.MimeMatch
                        Text
                        (case shellEscape "\"\$file\"")
                        (config.mimeMatcher pkgs special)
                  }}
              esac
              ''

in  xdg-open
