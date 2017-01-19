{ lib, runCommand, ghcWithPackages }:

let
  name = "warpspeed-1.0";

  script = builtins.toFile "${name}.hs" ''
    {-# LANGUAGE OverloadedStrings #-}
    module Main where

    import Safe
    import System.Environment (getArgs)
    import System.Exit (die)
    import Network.Wai
    import Network.Wai.Middleware.Static
    import Network.Wai.Handler.Warp
    import Network.HTTP.Types.Status

    main :: IO ()
    main = do
      args <- getArgs
      port <- case headMay args >>= readMay of
        Just p -> pure $ p
        Nothing -> die "please specify a port"
      runEnv port $ static $ \_ resp -> resp $ responseLBS notFound404 [] ""
   '';

   deps = hp: with hp; [ wai-middleware-static warp safe ];

in runCommand name {
  meta.description = "Trivial and very fast static HTTP file server";
} ''
  mkdir -p $out/bin
  ${ghcWithPackages deps}/bin/ghc -O2 -Wall -o "$out/bin/warpspeed" ${script}
''
