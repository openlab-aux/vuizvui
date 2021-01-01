# wrappers around execline commands ensuring sane default behaviours
{ writeExecline }:

let
  backtick = {
    var,
    cmd,
    importVar ? [ "importas" "-ui" var var ]
  }: writeExecline "nix-backtick" {}
    ([ "backtick" "-in" var cmd ]
    ++ importVar
    ++ [ "$@" ]);

in {
 inherit backtick;
}
