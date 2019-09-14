let Command = ../../Command/type.dhall

let Argument = ../../Argument/type.dhall

in    λ(a : { cmd : Text, description : Text })
    → { cmd =
          a.cmd
      , description =
          a.description
      , condition =
          None (Command Argument)
      , short-option =
          None Text
      , long-option =
          None Text
      , long-option-old-style =
          None Text
      , argument =
          None Text
      , keep-order =
          False
      , no-files =
          True
      , require-parameter =
          False
      , wraps =
          None Text
      }
