{ pkgs, rust-deps, writeRustSimple, writeExecline, getBins }:

let
  bins = getBins pkgs.coreutils [ "date" "cat" ];

  mustache-interpol = writeRustSimple "mustache-interpol" {
    dependencies = [
      rust-deps.mustache
      rust-deps.toml
      rust-deps.regex
      rust-deps.lazy_static
    ];
  } (./text-letter.rs);

  text-letter = writeExecline "write-letter" {} [
    "pipeline" [ bins.cat "/home/philip/kot/work/pa/krankenkasse/2021-05-18-tk-anschreiben-assurance-maladie.toml" ]
    mustache-interpol
  ];

in {
   inherit
     mustache-interpol
     text-letter
     ;
}
