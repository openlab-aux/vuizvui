{ pkgs, writeRustSimpleLib, writeRustSimple, el-semicolon, el-exec }:

let
  version-check = pkgs.buildRustCrate {
    pname = "version_check";
    version = "0.9.4";
    crateName = "version_check";
    sha256 = "sha256-gqwGGKrA2DYqteb1R+yew32P9jE/mKVdSC/VmuEGIy8=";
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

  netencode-rs-common = tests: writeRustSimpleLib "netencode" {
    dependencies = [ nom ];
    buildTests = tests;
    release = false;
    verbose = true;
  } ./netencode.rs;

  netencode-rs-tests = netencode-rs-common true;

  netencode-rs = netencode-rs-common false;

  record-get = writeRustSimple "record-get" {
    dependencies = [ netencode-rs el-semicolon ];
    # TODO: for some reason the skarnet linker flag
    # is propagated by the link target is not?
  } ./record-get.rs;


in {
  inherit
   netencode-rs
   netencode-rs-tests
   record-get
   ;
}
