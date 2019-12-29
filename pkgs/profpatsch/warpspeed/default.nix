{ lib, runCommand, ghcWithPackages }:

let
  name = "warpspeed-1.1";

  script = builtins.toFile "${name}.hs" ''
    {-# LANGUAGE OverloadedStrings #-}
    module Main where

    import Safe
    import Data.String (fromString)
    import Data.List (intercalate)
    import System.Environment (getArgs)
    import System.Exit (die)
    import Network.Wai
    import Network.Wai.Middleware.Static
    import Network.Wai.Handler.Warp
    import Network.HTTP.Types.Status
    import qualified Debug.Trace

    usage :: IO ()
    usage = die $ intercalate "\n"
      [ "usage: warpspeed <host> <port> [root-redirect]"
      , ""
      , "<host>: `*6` means any host, IPv6 preferred."
      , "See https://hackage.haskell.org/package/warp-3.3.5/docs/Network-Wai-Handler-Warp.html#t:HostPreference for the host binding syntax."
      ]

    rootRedirectPolicy :: String -> Policy
    rootRedirectPolicy redirTo = policy (\s -> Just $ if (Debug.Trace.traceShowId s) == "" then redirTo else s)

    main :: IO ()
    main = do
      args <- getArgs
      let portOrUsage port act = maybe usage act (readMay port :: Maybe Int)
      case args of
        [] -> usage
        [_] -> usage
        [ host, port ] -> portOrUsage port $ \p -> serve host p Nothing
        [ host, port, redirectTo ] -> portOrUsage port $ \p -> serve host p (Just redirectTo)
        _ -> usage
      where
        settings host port =
            setPort port
          $ setHost (fromString host)
          $ defaultSettings
        serve host port redirectTo =
            runSettings (settings host port)
          $ staticPolicy (maybe mempty rootRedirectPolicy redirectTo)
          $ \_ resp -> resp $ responseLBS notFound404 [] ""
   '';

   deps = hp: with hp; [ wai-middleware-static warp safe ];

in runCommand name {
  meta.description = "Trivial and very fast static HTTP file server";
} ''
  mkdir -p $out/bin
  ${ghcWithPackages deps}/bin/ghc -O2 -Wall -o "$out/bin/warpspeed" ${script}
''
