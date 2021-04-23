{ pkgs, writeRustSimpleLib, rust-deps }:

let
  el-semicolon = writeRustSimpleLib "el_semicolon" {
    release = false;
    verbose = true;
  } ./el_semicolon.rs;

  el-exec = writeRustSimpleLib "el_exec" {
    dependencies = [ rust-deps.libc ];
    buildInputs = [ pkgs.skalibs ];
    release = false;
    verbose = true;
  } ./el_exec.rs;

  el-substitute = writeRustSimpleLib "el_substitute" {
    dependencies = [ rust-deps.libc rust-deps.errno ];
    buildInputs = [ pkgs.skalibs pkgs.execline ];
    release = false;
    verbose = true;
  } ./el_substitute.rs;

in {
  inherit
    el-semicolon
    el-exec
    el-substitute
    ;
}
