let Void = ../imports/Void.dhall

let Option = ./Option/type.dhall

let Command = ./Command/type.dhall

let opt =
        λ(long : Text)
      → λ(description : Text)
      → { long =
            long
        , description =
            description
        , argument =
            None Text
        , short =
            None Text
        }

let fileOpt =
        opt "file" "Read expression from a file instead of standard input"
      ⫽ { argument = Some "FILE" }

let alphaOpt = opt "alpha" "α-normalize expression"

let inplaceOpt =
        opt "inplace" "Modify the specified file in-place"
      ⫽ { argument = Some "FILE" }

let jsonOpt = opt "json" "Use JSON representation of CBOR"

let leafCommand =
        λ(name : Text)
      → λ(description : Text)
      → { options =
            [] : List Option
        , subcommands =
            [] : List Void
        , name =
            name
        , description =
            description
        }

in    { name =
          "dhall"
      , description =
          "Interpreter for the Dhall language"
      , options =
          [ opt "annotate" "Add a type annotation to the output"
          , alphaOpt
          , opt "explain" "Explain error messages in more detail"
          , opt "plain" "Disable syntax highlighting"
          , opt "ascii" "Format code using only ASCII syntax"
          ,   opt "standard-version" "The standard version to use"
            ⫽ { argument = Some "X.Y.Z" }
          ]
      , subcommands =
          [ leafCommand
            "version"
            "Display version"
          ,   leafCommand "resolve" "Resolve an expression's imports"
            ⫽ { options =
                  [ fileOpt
                  , opt "dot" "Output import dependency graph in dot format"
                  , opt
                    "immediate-dependencies"
                    "List immediate import dependencies"
                  , opt
                    "transitive-dependencies"
                    "List transitive import dependencies"
                  ]
              }
          ,   leafCommand "type" "Infer an expression's type"
            ⫽ { options = [ fileOpt ] }
          ,   leafCommand "normalize" "Normalize an expression"
            ⫽ { options = [ fileOpt, alphaOpt ] }
          , leafCommand "repl" "Interpret expressions in a REPL"
          , leafCommand
            "diff"
            "Render the difference between the normal form of two expressions"
          , leafCommand "hash" "Compute semantic hashes for Dhall expressions"
          ,   leafCommand "lint" "Improve Dhall code"
            ⫽ { options = [ inplaceOpt ] }
          ,   leafCommand "format" "Formatter for the Dhall language"
            ⫽ { options =
                  [ opt "check" "Only check if the input is formatted"
                  , inplaceOpt
                  ]
              }
          ,   leafCommand
              "freeze"
              "Add integrity checks to remote import statements of an expression"
            ⫽ { options =
                  [ inplaceOpt
                  , opt
                    "all"
                    "Add integrity checks to all imports (not just remote imports)"
                  , opt
                    "cache"
                    "Add fallback unprotected imports when using integrity checks purely for caching purposes"
                  ]
              }
          ,   leafCommand "encode" "Encode a Dhall expression to binary"
            ⫽ { options = [ fileOpt, jsonOpt ] }
          ,   leafCommand "decode" "Decode a Dhall expression from binary"
            ⫽ { options = [ fileOpt, jsonOpt ] }
          ,   leafCommand
              "text"
              "Render a Dhall expression that evaluates to a Text literal"
            ⫽ { options = [ fileOpt ] }
          ]
      }
    : Command (Command Void)
