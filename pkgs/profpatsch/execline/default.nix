{ pkgs, writeRustSimpleLib, rust-deps }:

let
  el-semicolon-common = tests: writeRustSimpleLib "el_semicolon" {
    buildTests = tests;
    release = false;
    verbose = true;
  } ./el_semicolon.rs;

  el-semicolon-tests = el-semicolon-common true;
  el-semicolon = el-semicolon-common false;

  el-exec = writeRustSimpleLib "el_exec" {
    dependencies = [ rust-deps.libc ];
    buildInputs = [ pkgs.skalibs ];
    release = false;
    verbose = true;
  } ./el_exec.rs;

  el-substitute = writeRustSimpleLib "el_substitute" {
    dependencies = [ rust-deps.libc rust-deps.errno ];
    buildInputs = [ pkgs.skalibs ];
    release = false;
    verbose = true;
  } ./el_substitute.rs;

in {
  inherit
    el-semicolon
    el-semicolon-tests
    el-exec
    el-substitute
    ;
}
