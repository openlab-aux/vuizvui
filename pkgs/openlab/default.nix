{ callPackage, haskell }:

{
  gitit = callPackage ./gitit { hlib = haskell.lib; };
  # TODO: fix haskell code
  # stackenblocken = callPackage ./stackenblocken {};
}
