{ writeRustSimpleLib }:

let
  el-semicolon-common = tests: writeRustSimpleLib "el_semicolon" {
    buildTests = tests;
    release = false;
    verbose = true;
  } ./el_semicolon.rs;

  el-semicolon-tests = el-semicolon-common true;

  el-semicolon = el-semicolon-common false;

in {
  inherit
    el-semicolon
    el-semicolon-tests
    ;
}
