{ pkgs, lib, toNetstring, writeExecline, runExecline, getBins, writeRustSimple, netencode-rs, el-semicolon, el-substitute, el-exec, netencode }:

let
  bins = getBins pkgs.coreutils [ "ln" "mkdir" "echo" "printenv" "cat" "env" ]
    // getBins pkgs.fdtools [ "multitee" ];

  jquery = pkgs.fetchurl {
    url = "https://code.jquery.com/jquery-3.5.1.min.js";
    sha256 = "0gbfbfcbcpl8nq2shknsyz5pirf5wbnb54m3dynxs68x9y4sbxpp";
  };

  joinPath = lib.concatStringsSep "/";

  stderr-tee = writeExecline "stderr-tee" {} [
    "pipeline" [ bins.multitee "0-1,2" ] "$@"
  ];

  staticFiles =
    let
      jsJquery = {
        relativeDir = [ "js" ];
        relativeFile = "jquery.js";
        path = jquery;
      };
      cssNormalize = {
        relativeDir = [ "css" ];
        relativeFile = "normalize.css";
        path = ./normalize.css;
      };
      cssMain = {
        relativeDir = [ "css" ];
        relativeFile = "main.css";
        path = ./main.css;
      };
      cv_pdf = {
        relativeDir = [];
        relativeFile = "cv.pdf";
        path = ./cv.pdf;
      };
      id_txt = {
        relativeDir = [];
        relativeFile = "id.txt";
        path = ./id.txt;
      };
      index_html = {
        relativeDir = [];
        relativeFile = "index.html";
        path = pkgs.writeText "index.html"
          (import ./index.html.nix
            (lib.mapAttrs
              (k: v: joinPath (v.relativeDir ++ [ v.relativeFile ]))
              {
                inherit jsJquery cssNormalize cssMain;
                inherit cv_pdf id_txt;
              }));
      };
    in {
      inherit jsJquery cssNormalize cssMain;
      inherit cv_pdf id_txt index_html;
    };

  record-get = writeRustSimple "record-get" {
    dependencies = [ netencode-rs el-semicolon el-exec ];
    # TODO: for some reason the skarnet linker flag
    # is propagated by the link target is not?
    buildInputs = [ pkgs.skalibs ];
  } ./record-get.rs;

  importas-if = writeRustSimple "importas-if" {
    dependencies = [ netencode-rs el-substitute el-exec ];
    # TODO: for some reason the skarnet linker flag
    # is propagated by the link target is not?
    buildInputs = [ pkgs.skalibs pkgs.execline ];
  } (pkgs.writeText "importas-if.rs" ''
  extern crate el_exec;
  extern crate el_substitute;
  use std::ffi::CString;
  use std::ffi::{OsString, OsStr};
  use std::os::unix::ffi::{OsStringExt, OsStrExt};
  fn main() {
    let args = std::env::args_os().into_iter()
      .map(|arg| CString::new(arg.as_bytes()).unwrap())
      .collect::<Vec<CString>>();
    assert!(args.len() >= 3, "at least two arguments required");
    let import = args[2].as_bytes();
    let as_var = &args[1];
    match std::env::var_os(OsStr::from_bytes(import)) {
      // If envar doesn’t exist, exit 1.
      // This makes it possible to react outside
      None => std::process::exit(1),
      // If it does, continue
      Some(val) =>
        el_exec::xpathexec0(
          &el_substitute::simple_substitute_argv(
            &vec![el_substitute::Subst {
              var: &as_var,
              value: &CString::new(val.as_bytes()).unwrap(),
            }],
            &args[3..]
          )
        )
    }
  }
  '');

  linkStaticFiles = files: runExecline "link-static-files" {
    stdin = lib.concatStrings (
      lib.mapAttrsToList (_: static:
         toNetstring
           (netencode.record
             ((lib.optional (static.relativeDir != []) { key = "relDir"; val = netencode.binary (joinPath static.relativeDir); })
             ++ [
             { key = "relFile"; val = netencode.binary static.relativeFile; }
             { key = "path"; val = netencode.binary static.path; }
           ])))
        files
    );
  } [
    "importas" "-ui" "out" "out"
    "if" [ bins.mkdir "$out" ]
    "forstdin" "-d" "" "dict"
    "pipeline" [ bins.printenv "dict" ]
    record-get [ "relDir" "relFile" "path" ]
    "importas" "-ui" "relFile" "relFile"
    "importas" "-ui" "path" "path"
    "if" "-n" [
      importas-if "relDir" "relDir"
      "if" [ bins.mkdir "-p" "\${out}/\${relDir}" ]
      bins.ln "-s" "$path" "\${out}/\${relDir}/\${relFile}"
    ]
    bins.ln "-s" "$path" "\${out}/\${relFile}"
  ];

  websiteStatic = linkStaticFiles staticFiles;

in {
  inherit
    websiteStatic
    record-get
    importas-if
    ;
}
