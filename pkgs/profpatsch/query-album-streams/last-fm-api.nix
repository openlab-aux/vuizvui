{ pkgs, getBins, writeExecline, writeHaskellInterpret }:
let

  lib = pkgs.lib;
  bins = getBins pkgs.httpie [ "http" ];

  develop = true;

  writeHaskell = name: { interpret ? false, withPackages }:
    if interpret
    then writeHaskellInterpret name { inherit withPackages; }
    else pkgs.writers.writeHaskell name { libraries = withPackages pkgs.haskellPackages; };

  # see https://hackage.haskell.org/package/aeson-schema-0.4.1.2/docs/Data-Aeson-Schema-Types.html#t:Schema
  # for the schema format.
  # The input is a json-encoding of that via quasi-quoter.
  json-schema-validator = name: schema: writeHaskell name {
    withPackages = hps: [ (pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.markUnbroken hps.aeson-schema)) hps.aeson ];
    interpret = develop;
  } ''
    {-# language QuasiQuotes #-}
    import qualified Data.ByteString.Lazy as BS
    import Data.Aeson (eitherDecode')
    import Data.Aeson.Schema
    import System.Exit (die, exitSuccess)

    main :: IO ()
    main = do
      stdin <- BS.getContents
      case (eitherDecode' stdin) of
        Left errs ->  die errs
        Right json -> do
          let val = validate mempty schema json
          if val == []
          then BS.putStr stdin >> exitSuccess
          else die (show val)

    schema :: Schema ()
    schema = [schemaQQ| ${lib.generators.toJSON {} schema} |]
  '';

  query-lastFm-album = writeExecline "query-lastFm-album" { readNArgs = 2; } [
    bins.http
      "GET"
      "https://ws.audioscrobbler.com/2.0/"
      "--"
      "api_key==\${1}"
      "method==album.search"
      "album==\${2}"
      "format==json"
  ];

  schema = {
    obj = { required ? true }: propsMap: {
      type = "object";
      inherit required;
      properties = propsMap;
    };

    arr = { required ? true }: itemsSchema: {
      type = "array";
      inherit required;
      items = itemsSchema;
    };
  };

  validator = json-schema-validator "last-fm-album-output" (with schema; obj {} {
    results = obj {} {
      albummatches = obj {} {
        album = arr {} (obj {} {
          name = { type = "string"; };
          artist = { type = "string"; };
        });
      };
    };
  });

  query-and-validate = writeExecline "validate-lastfm-album-response" { } [
    "pipeline" [ query-lastFm-album "$@" ]
    "if" [ validator ]
  ];


# in validator
in {
  inherit query-lastFm-album validator query-and-validate;
}
