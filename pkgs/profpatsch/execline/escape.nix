{ lib }:
let
  # replaces " and \ to \" and \\ respectively and quote with "
  # e.g.
  #   a"b\c -> "a\"b\\c"
  #   a\"bc -> "a\\\"bc"
  # TODO upsteam into nixpkgs
  escapeExeclineArg = arg:
    ''"${builtins.replaceStrings [ ''"'' ''\'' ] [ ''\"'' ''\\'' ] (toString arg)}"'';

  # Escapes an execline (list of execline strings) to be passed to execlineb
  # Give it a nested list of strings. Nested lists are interpolated as execline
  # blocks ({}).
  # Everything is quoted correctly.
  #
  # Example:
  #   escapeExecline [ "if" [ "somecommand" ] "true" ]
  #   == ''"if" { "somecommand" } "true"''
  escapeExecline = execlineList: lib.concatStringsSep " "
    (let
      go = arg:
        if      builtins.isString arg then [(escapeExeclineArg arg)]
        else if builtins.isPath arg then [(escapeExeclineArg "${arg}")]
        else if lib.isDerivation arg then [(escapeExeclineArg arg)]
        else if builtins.isList arg then [ "{" ] ++ builtins.concatMap go arg ++ [ "}" ]
        else abort "escapeExecline can only hande nested lists of strings, was ${lib.generators.toPretty {} arg}";
     in builtins.concatMap go execlineList);

in {
  inherit escapeExecline;
}
