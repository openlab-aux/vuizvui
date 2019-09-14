let Command = ./Command/type.dhall

let Argument = ./Argument/type.dhall

let Complete = ./Fish/Complete/type.dhall

let argCommandToList
    : Command Argument → List Text
    = ./Command/toList.dhall
      Argument
      (./Argument/toArgList.dhall ./OptionPrinter/newStyle.dhall)

let complete = ./Fish/Complete/default.dhall

let completeToCommand
    : Complete → Command Argument
    = ./Fish/Complete/toCommand.dhall ./OptionPrinter/newStyle.dhall

in  let fishSeenSubcommandFn = "__fish_seen_subcommand_from"

    let fishUseSubcommandFn = "__fish_use_subcommand"

    let fooSubcommand
        : Command Argument
        = completeToCommand
          (   complete { cmd = "abc", description = "this is foo option" }
            ⫽ { condition =
                  Some { cmd = fishUseSubcommandFn, args = [] : List Argument }
              , argument =
                  Some "foo"
              }
          )

    let fooSubcommandBarOption
        : Command Argument
        = completeToCommand
          (   complete { cmd = "abc", description = "will bar the baz" }
            ⫽ { condition =
                  Some
                  { cmd =
                      fishSeenSubcommandFn
                  , args =
                      [ Argument.Plain "foo" ]
                  }
              , long-option =
                  Some "bar"
              , short-option =
                  Some "b"
              }
          )

    in    [ argCommandToList fooSubcommand
          , argCommandToList fooSubcommandBarOption
          , [ "complete", "--do-complete=abc foo -" ]
          ]
        : List (List Text)
