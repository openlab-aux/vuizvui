{ lib, ... }:

let
  toNetstring = s:
    "${toString (builtins.stringLength s)}:${s},";

  toNetstringList = xs:
    lib.concatStrings (map toNetstring xs);

  toNetstringKeyVal = attrs:
    lib.concatStrings
      (lib.mapAttrsToList
        (k: v: toNetstring (toNetstring k + toNetstring v))
        attrs);

in {
  inherit toNetstring toNetstringList toNetstringKeyVal;
}
