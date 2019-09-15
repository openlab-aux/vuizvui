let Void = ./imports/Void.dhall

let Prelude = ./imports/Prelude.dhall

let Command = ./Command/type.dhall

let Argument = ./Argument/type.dhall

let FishComplete = ./Fish/Complete/type.dhall

let argCommandToList
    : Command Argument → List Text
    = ./Command/toList.dhall
      Argument
      (./Argument/toArgList.dhall ./OptionPrinter/newStyle.dhall)

let complete = ./Fish/Complete/default.dhall

let completeToCommand
    : FishComplete → Command Argument
    = ./Fish/Complete/toCommand.dhall ./OptionPrinter/newStyle.dhall

let Completion = ./Completion/package.dhall

let InSubcommand = < No | ToplevelSpecial | Subcommand : Text >

in  let fishSeenSubcommandFn = "__fish_seen_subcommand_from"

    let fishUseSubcommandFn = "__fish_use_subcommand"

    let fishCommandLineExactlyFn = "__fish_command_line_exactly"

    let subcommandCond =
            λ(programName : Text)
          → λ(inSubcommand : InSubcommand)
          → merge
            { ToplevelSpecial =
                Some
                { cmd =
                    fishCommandLineExactlyFn
                , args =
                    [ Argument.Plain programName ]
                }
            , Subcommand =
                  λ(sub : Text)
                → Some
                  { cmd = fishSeenSubcommandFn, args = [ Argument.Plain sub ] }
            , No =
                None (Command Argument)
            }
            inSubcommand
            : Optional (Command Argument)

    let optionsComplete =
            λ(programName : Text)
          → λ(inSubcommand : InSubcommand)
          → λ(options : List Completion.Option)
          → let optcompl =
                    λ(option : Completion.Option)
                  →   complete
                      { cmd = programName, description = option.description }
                    ⫽ { condition =
                          subcommandCond programName inSubcommand
                      , short-option =
                          option.short
                      , long-option =
                          Some option.long
                      }

            in  Prelude.List.map Completion.Option FishComplete optcompl options

    let mergeCommandArguments =
            λ(programName : Text)
          → λ(inSubcommand : InSubcommand)
          → λ(a : Type)
          → λ(f : List a → List FishComplete)
          → λ(arguments : Completion.Command.Arguments a)
          → let filesBlock =
                    complete { cmd = programName, description = "" }
                  ⫽ { condition =
                        subcommandCond programName inSubcommand
                    , no-files =
                        True
                    }

            in  merge
                { Subcommands =
                    λ(cmds : List a) → f cmds # [ filesBlock ]
                , Files =
                    [] : List FishComplete
                }
                arguments
                : List FishComplete

    let subcommandToFishComplete =
            λ(programName : Text)
          → λ(command : Completion.Command.Type Void)
          → let subcommandComplete =
                    [   complete
                        { cmd = programName, description = command.description }
                      ⫽ { condition =
                            Some
                            { cmd =
                                fishUseSubcommandFn
                            , args =
                                [] : List Argument
                            }
                        , argument =
                            Some command.name
                        , no-files =
                            False
                        }
                    ]
                  # optionsComplete
                    programName
                    (InSubcommand.Subcommand command.name)
                    command.options

            in    subcommandComplete
                # mergeCommandArguments
                  programName
                  (InSubcommand.Subcommand command.name)
                  Void
                  (λ(_ : List Void) → [] : List FishComplete)
                  command.arguments

    let simpleCommandToFishComplete =
            λ ( c
              : { command :
                    Completion.Command.Type (Completion.Command.Type Void)
                , toplevelCommandIsSubcommand :
                    Bool
                }
              )
          →   optionsComplete c.command.name InSubcommand.No c.command.options
            # mergeCommandArguments
              c.command.name
              (       if c.toplevelCommandIsSubcommand

                then  InSubcommand.ToplevelSpecial

                else  InSubcommand.No
              )
              (Completion.Command.Type Void)
              ( Prelude.List.concatMap
                (Completion.Command.Type Void)
                FishComplete
                (subcommandToFishComplete c.command.name)
              )
              c.command.arguments

    in    Prelude.List.map
          FishComplete
          (List Text)
          (λ(c : FishComplete) → argCommandToList (completeToCommand c))
          (simpleCommandToFishComplete ./Completion/completion.dhall)
        # [ [ "complete", "--do-complete=dhall " ] ]
