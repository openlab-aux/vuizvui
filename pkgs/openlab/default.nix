{ callPackage, haskell }:

{
  gitit = callPackage ./gitit { hlib = haskell.lib; };
  stackenblocken = callPackage ./stackenblocken {};
}
