let Prelude = ../../imports/Prelude.dhall

let Command = ../../Command/type.dhall

let Argument = ../../Argument/type.dhall

let Option = ../../Option/type.dhall

let OptionPrinter = ../../OptionPrinter/type.dhall

let Complete = ./type.dhall

in    λ(conditionOptionPrinter : OptionPrinter)
    → λ(c : Complete)
    → let long =
              λ(name : Text)
            → Prelude.Optional.map
              Text
              Argument
              (   λ(content : Text)
                → Argument.Option { opt = Option.Long name, arg = content }
              )

      let flag =
              λ(name : Text)
            → λ(flag : Bool)
            →       if flag

              then  Some (Argument.Flag (Option.Long name))

              else  None Argument

      let conditionToText =
              λ(c : Command Argument)
            →   Prelude.Text.concatSep
                " "
                ( ../../Command/toList.dhall
                  Argument
                  (../../Argument/toArgList.dhall conditionOptionPrinter)
                  c
                )
              : Text

      let args =
              [ long "command" (Some c.cmd)
              , long "description" (Some c.description)
              , Prelude.Optional.map
                (Command Argument)
                Argument
                (   λ(c : Command Argument)
                  → Argument.Option
                    { opt = Option.Long "condition", arg = conditionToText c }
                )
                c.condition
              , long "short-option" c.short-option
              , long "long-option" c.long-option
              , long "old-option" c.long-option-old-style
              , long "arguments" c.argument
              , long "wraps" c.wraps
              , flag "keep-order" c.keep-order
              , flag "no-files" c.no-files
              , flag "require-parameter" c.require-parameter
              ]
            : List (Optional Argument)

      let id = λ(a : Optional Argument) → a

      in  { cmd =
              "complete"
          , args =
                ../../List/filterOptional.dhall
                (Optional Argument)
                Argument
                id
                args
              : List Argument
          }
