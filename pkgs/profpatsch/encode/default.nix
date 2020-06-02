{ pkgs, writeRustSimpleLib }:

let
  version-check = pkgs.buildRustCrate {
    pname = "version-check";
    version = "0.9.2";
    crateName = "version-check";
    sha256 = "1vwvc1mzwv8ana9jv8z933p2xzgj1533qwwl5zr8mi89azyhq21v";
  };
  memchr = pkgs.buildRustCrate {
    pname = "memchr";
    version = "2.3.3";
    crateName = "memchr";
    sha256 = "1ivxvlswglk6wd46gadkbbsknr94gwryk6y21v64ja7x4icrpihw";
  };
  nom = pkgs.buildRustCrate {
    pname = "nom";
    version = "5.1.1";
    crateName = "nom";
    sha256 = "1gb4r6mjwd645jqh02nhn60i7qkw8cgy3xq1r4clnmvz3cmkv1l0";
    dependencies = [ memchr ];
    buildDependencies = [ version-check ];
    features = [ "std" "alloc" ];
  };

  encode-rs-common = tests: writeRustSimpleLib "encode" {
    dependencies = [ nom ];
    buildTests = tests;
    release = false;
    verbose = true;
  } ./encode.rs ;

  encode-rs-tests = encode-rs-common true;

  encode-rs = encode-rs-common false;

in {
  inherit
   encode-rs
   encode-rs-tests
   ;
}
