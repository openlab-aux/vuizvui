{ stdenv, drvSeqL, runExecline, bin
# https://www.mail-archive.com/skaware@list.skarnet.org/msg01256.html
, coreutils }:

let

  # lol
  writeScript = name: script: runExecline name {
    derivationArgs = {
      inherit script;
      passAsFile = [ "script" ];
      preferLocalBuild = true;
      allowSubstitutes = false;
    };
  } [
      "importas" "-ui" "s" "scriptPath"
      "importas" "-ui" "out" "out"
      "foreground" [
        "${coreutils}/bin/mv" "$s" "$out"
      ]
      "${bin.s6-chmod}" "0755" "$out"
  ];

  # execline block of depth 1
  block = args: builtins.map (arg: " ${arg}") args ++ [ "" ];

  # derivation that tests whether a given line exists
  # in the given file. Does not use runExecline, because
  # that should be tested after all.
  fileHasLine = line: file: derivation {
    name = "run-execline-test-file-${file.name}-has-line";
    inherit (stdenv) system;
    builder = bin.execlineIf;
    args =
      (block [
        bin.redirfd "-r" "0" file   # read file to stdin
        bin.s6-grep "-F" "-q" line   # and grep for the line
      ])
      ++ [
        # if the block succeeded, touch $out
        bin.importas "-ui" "out" "out"
        bin.s6-touch "$out"
      ];
    preferLocalBuild = true;
    allowSubstitutes = false;
  };

  # basic test that touches out
  basic = runExecline "run-execline-test-basic" {
    derivationArgs = {
      preferLocalBuild = true;
      allowSubstitutes = false;
    };
  } [
      "importas" "-ui" "out" "out"
      "${bin.s6-touch}" "$out"
  ];

  # whether the stdin argument works as intended
  stdin = fileHasLine "foo" (runExecline "run-execline-test-stdin" {
    stdin = "foo\nbar\nfoo";
    derivationArgs = {
      preferLocalBuild = true;
      allowSubstitutes = false;
    };
  } [
      "importas" "-ui" "out" "out"
      # this pipes stdout of s6-cat to $out
      # and s6-cat redirects from stdin to stdout
      "redirfd" "-w" "1" "$out" bin.s6-cat
  ]);

  wrapWithVar = runExecline "run-execline-test-wrap-with-var" {
    builderWrapper = writeScript "var-wrapper" ''
      #!${bin.execlineb} -S0
      export myvar myvalue $@
    '';
    derivationArgs = {
      preferLocalBuild = true;
      allowSubstitutes = false;
    };
  } [
    "importas" "-ui" "v" "myvar"
    "if" [ bin.s6-test "myvalue" "=" "$v" ]
      "importas" "out" "out"
      bin.s6-touch "$out"
  ];

in [ basic stdin wrapWithVar ]
