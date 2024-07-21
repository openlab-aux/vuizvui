module XdgOpen where

data Drv
type Executable = String

foreign import nixpkgs :: { hello :: Drv }
foreign import lib
  :: { map :: forall a b. (a -> b) -> Array a -> Array b
     , concatMap :: forall a b. (a -> Array b) -> Array a -> Array b
     , concatStringsSep :: String -> Array String -> String
     , concatStrings :: Array String -> String
     , concatMapStrings :: (String -> String) -> Array String -> String
     , genList :: forall a. (Int -> a) -> Int -> Array a
     }

foreign import appendString :: String -> String -> String
foreign import appendArray :: forall a. Array a -> Array a -> Array a
foreign import builtinThrow :: forall a. String -> a

throw :: forall a. String -> a
throw = builtinThrow

todo :: forall a. a
todo = builtinThrow "TODO: Unimplemented"

infixl 1 appendString as +
infixl 1 appendArray as ++

renderMime ∷ Array String → String
renderMime = lib.concatStringsSep "/"

repeatText :: Int -> String -> String
repeatText n s = lib.concatStrings (lib.genList (\_ -> s) n)

type Lines = { indent :: Int, lines :: Array String }

-- | Pretty print the line array with its indent.
prettyLines :: Lines -> String
prettyLines lines = lib.concatMapStrings (\line -> (lib.concatStrings [ repeatText lines.indent " ", line, "\n" ])) lines.lines

type Mime = Array String

type Arg = forall r. { string :: String -> r, variable :: String -> r } -> r

type CommandTemplate templates =
  { exe :: Executable, args :: templates → Array Arg }

-- Given an executable and args to pass to the executable,
-- which might be a bash variable or a simple command line string.
-- Should remove that indirection at some point and just generate execline strings/scripts instead. (?)
type Command =
  CommandTemplate Arg

type MimeMatch = { mime :: Mime, cmd :: Command }

-- | Escape the given shell command, at least the String arguments of it.
-- Passes `$file` as variable argument.
-- The final shell command is executed into.
shellEscapeExecCommand :: (String -> String) -> String -> Command -> String
shellEscapeExecCommand shellEscape file cmd =
  lib.concatStringsSep " "
    ( [ "exec", shellEscape cmd.exe ] ++ lib.map
        ( \(arg :: Arg) ->
            arg { string: \t -> shellEscape t, variable: \t -> t }
        )
        (cmd.args (\{ variable } -> variable file))
    )

mimeMatcherCase :: (String -> String) -> String -> MimeMatch -> Array String
mimeMatcherCase shellEscape file m =
  [ renderMime m.mime + ")"
  , shellEscapeExecCommand shellEscape file m.cmd
  , ";;"
  ]

type UriMimeGlob =
  { desc :: String
  , -- less specific than glob, used by firefox to refer to the schema
    schemaPrefix :: Array String
  , -- schema shell glob to check whether a link corresponds to the schema
    glob :: Array String
  , handler :: MimeMatch
  }

mimeGlobCase :: (String -> String) -> String -> UriMimeGlob -> Array String
mimeGlobCase shellEscape file g = lib.concatMap
  ( \(match :: String) ->
      [ match + ")"
      , shellEscapeExecCommand shellEscape file g.handler.cmd
      , ";;"
      ]
  )
  g.glob

-- type Special =
--   { openInEditor :: Command
--   , openInBrowser :: Command
--   , fetchHttpUrlMime :: Command
--   , composeMailTo :: Command
--   , execInTerminalEmulator :: Command -> Command
--   , dmenuListBinariesAndExec :: Command
--   , notify :: String → Command
--   }

type Config =
  { uriMimeGlobs :: Array UriMimeGlob
  , orderedMimeMatchers :: Array MimeMatch
  }

xdgOpen
  :: (String -> String -> Executable)
  -> (String -> String)
  -> Config
  -> Executable
xdgOpen writeDash shellEscape config = writeDash "xdg-open"
  ( lib.concatStringsSep "\n"
      [
        -- partially taken from
        -- https://github.com/march-linux/mimi/blob/master/xdg-open
        "set -e"
      , "file=$1"
      , "mime="
      ,
        -- TODO: --dry-run to display what would be opened and why
        "notify-send --expire-time=500 -- \\\"xdg-open: $1\\\""
      , "# match on protocols; if you want to match files reliably, start with file://"
      , "case \\\"$file\\\" in"
      , prettyLines
          { indent: 2
          , lines: lib.concatMap (mimeGlobCase shellEscape "\\\"$file\\\"") config.uriMimeGlobs
          }
      , "*)"
      , "  # it’s a file"
      , "  # strip possible protocol"
      , "  file=\\${file#file://}"
      , "  mime=$(file -E --brief --mime-type \\\"$file\\\") \\\\"
      , "    || (echo \\\"$mime\\\" 1>&2; exit 1)"
      , "  # ^ echo the error message of file"
      , "  ;;"
      , "esac"
      , ""
      , "case \\\"$mime\\\" in"
      , prettyLines
          { indent: 2
          , lines: lib.concatMap (mimeMatcherCase shellEscape "\\\"$file\\\"") config.orderedMimeMatchers

          }
      , "esac"
      ]

  )

type Opts =
  { uriMimeGlobs :: Array UriMimeGlob
  , orderedMimeMatchers :: Array MimeMatch
  , writeDash :: String -> String -> String
  }

main
  :: Opts
  -> String
main opts = xdgOpen opts.writeDash (\txt -> txt)
  { uriMimeGlobs: opts.uriMimeGlobs
  , orderedMimeMatchers:
      opts.orderedMimeMatchers
  }
