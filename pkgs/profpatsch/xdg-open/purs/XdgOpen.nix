let
  # TODO: use nixpkgs from packageset
  nixpkgs = import <nixpkgs> {};

in {
  lib = nixpkgs.lib;
  appendString = s: s2: s + s2;
  appendArray = a: a2: a ++ a2;
  builtinThrow = msg: throw msg;
}
