{ pkgs, tvl, lib, toNetstring, toNetstringList, writeExecline, runExecline, getBins, writeRustSimple, netencode-rs, el-semicolon, el-substitute, el-exec, netencode, record-get }:

let
  bins = getBins pkgs.coreutils [ "ln" "mkdir" "echo" "printenv" "cat" "env" ]
    // getBins pkgs.fdtools [ "multitee" ];

  quattrocento-latin = pkgs.fetchurl {
    url = "https://fonts.gstatic.com/s/quattrocento/v11/OZpEg_xvsDZQL_LKIF7q4jP3w2j6.woff2";
    sha256 = "161dzd0az6zw8js1q8ikf4yhm0h9zidc5wqlnsrpzw5npdzmbzbi";
  };

  open-sans-latin = pkgs.fetchurl {
    url = "https://fonts.gstatic.com/s/opensans/v17/mem5YaGs126MiZpBA-UN_r8OUuhp.woff2";
    sha256 = "1qj6qwajfjcs4l0mwj5lj7jdbc35y3rkqrsz2w41zcfzh8nywxzn";
  };

  joinPath = lib.concatStringsSep "/";

  stderr-tee = writeExecline "stderr-tee" {} [
    "pipeline" [ bins.multitee "0-1,2" ] "$@"
  ];

  applyTemplate = {
    name,
    # the nix template file to import
    templateNix,
    # the page dependencies, attrset of path, relativeDir and relativeFile
    pageDeps,
    # html that should just be interpolated;
    # will shadow any attrs in pageDeps of the same name
    interpolateHtml ? {},
  }:
    pkgs.writeText name
      (import templateNix
        ((lib.mapAttrs
            # relative to the root of the webpage
            (k: v: "/" + (joinPath (v.relativeDir ++ [ v.relativeFile ])))
            pageDeps)
          // interpolateHtml));

  staticFiles =
    rec {
      jsTalkies = {
        relativeDir = [ "js" ];
        relativeFile = "talkies.js";
        path = ./talkies.js;
      };
      cssNormalize = {
        relativeDir = [ "css" ];
        relativeFile = "normalize.css";
        path = ./normalize.css;
      };
      cssMain = {
        relativeDir = [ "css" ];
        relativeFile = "main.css";
        path = applyTemplate {
          name = "main.css";
          templateNix = ./main.css.nix;
          pageDeps = {
            inherit
              fontsQuattrocentoLatin
              fontsOpenSansLatin
              ;
          };
        };
      };
      cssConcatenated =
        let
          concatCss = cssFiles: runExecline "concat-css" {
            stdin = toNetstringList (map (c: c.path) cssFiles);
          } [
            "importas" "out" "out"
            "forstdin" "-Ed" "" "css"
            "redirfd" "-a" "1" "$out"
            "redirfd" "-r" "0" "$css"
            bins.cat
          ];
        in {
          relativeDir = [ "css" ];
          relativeFile = "concatenated.css";
          path = concatCss [cssNormalize cssMain];
        };

      fontsQuattrocentoLatin = {
        relativeDir = [ "fonts" ];
        relativeFile = "quattrocento-latin.woff2";
        path = quattrocento-latin;
      };
      fontsOpenSansLatin = {
        relativeDir = [ "fonts" ];
        relativeFile = "open-sans-latin.woff2";
        path = open-sans-latin;
      };
      id_txt = {
        relativeDir = [];
        relativeFile = "id.txt";
        path = ./id.txt;
      };
      key_asc = {
        relativeDir = [];
        relativeFile = "key.asc";
        path = ./key.asc;
      };
      index_html = {
        relativeDir = [];
        relativeFile = "index.html";
        path = applyTemplate {
          name = "index.html";
          templateNix = ./index.html.nix;
          pageDeps = {
            inherit jsTalkies;
            inherit cssNormalize cssMain;
            inherit id_txt;
            # preloading
            inherit
              fontsQuattrocentoLatin
              fontsOpenSansLatin
              ;
          };
          interpolateHtml = {
            notes-html-snippet = tvl.users.Profpatsch.blog.notes-index-html;
            projects-html-snippet = tvl.users.Profpatsch.blog.projects-index-html;
            posts-html-snippet = tvl.users.Profpatsch.blog.posts-index-html;
          };
        };
      };
      toc = {
        relativeDir = [];
        relativeFile = "toc";
        path = ./toc.txt;
      };
    };

  concatenatedCss =
    let mkRoute = css: { route = css.relativeDir ++ [ css.relativeFile ]; };
    in mkRoute staticFiles.cssConcatenated;


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
        el_exec::xmexec0(
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
    concatenatedCss
    ;
}
