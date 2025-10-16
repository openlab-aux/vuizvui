module Main where

import Config
import XdgOpen (Arg(..), Command, CommandTemplate, lib, xdgOpen)

type ArgR r = { string :: String -> r, variable :: String -> r } -> r
type Command2 = CommandTemplate (ArgR Arg)

type Opts =
  { writeDash :: String -> String -> String
  , escapeShellArg :: String -> String
  , pkgs :: Pkgs
  , special :: Special Command2
  }

main :: Opts -> String
main opts =
  xdgOpen opts.writeDash opts.escapeShellArg (mime opts.pkgs (special2ToSpecial opts.special))

command2ToCommand :: Command2 -> Command
command2ToCommand cmd2 =
  { exe: cmd2.exe
  , args:
      let
        conv = convArg
      in
        \arg -> lib.map conv.arg2ToArg (cmd2.args (conv.argToArg2 arg))
  }

commandToCommand2 :: Command -> Command2
commandToCommand2 cmd =
  { exe: cmd.exe
  , args:
      let
        conv = convArg
      in
        \arg2 -> lib.map conv.argToArg2 (cmd.args (conv.arg2ToArg arg2))
  }

convArg :: { arg2ToArg :: ArgR Arg -> Arg, argToArg2 :: Arg -> ArgR Arg }
convArg =
  { arg2ToArg: \arg2 ->
      arg2
        { string: \t -> ArgString t
        , variable: \t -> ArgVariable t
        }
  , argToArg2: \arg -> case arg of
      ArgString t -> \{ string } -> string t
      ArgVariable t -> \{ variable } -> variable t

  }

special2ToSpecial :: Special Command2 -> Special Command
special2ToSpecial special =
  { openInBrowser: command2ToCommand special.openInBrowser
  , fetchHttpUrlMime: command2ToCommand special.fetchHttpUrlMime
  , composeMailTo: command2ToCommand special.composeMailTo
  , execInTerminalEmulator: \cmd -> command2ToCommand (special.execInTerminalEmulator (commandToCommand2 cmd))
  , dmenuListBinariesAndExec: command2ToCommand special.dmenuListBinariesAndExec
  , notify: \str -> command2ToCommand (special.notify str)
  }

