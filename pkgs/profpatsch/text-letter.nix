{ pkgs, rust-deps, writeRustSimple, writeExecline, getBins }:

let
  bins = getBins pkgs.coreutils [ "date" "cat" ]
      // getBins pkgs.paps [ "paps" ]
      // getBins pkgs.ghostscript [ "ps2pdf" ];

  mustache-interpol = writeRustSimple "mustache-interpol" {
    dependencies = [
      rust-deps.mustache
      rust-deps.toml
      rust-deps.regex
      rust-deps.lazy_static
    ];
  } (./text-letter.rs);

  text-letter = writeExecline "write-letter" {} [
    mustache-interpol
  ];

  text-letter-pdf = writeExecline "write-letter-pdf" {} [
    "pipeline" [ text-letter ]
    "pipeline" [ bins.paps "--encoding=utf8" ]
    bins.ps2pdf "-"
  ];

in {
   inherit
     mustache-interpol
     text-letter
     text-letter-pdf
     ;
}
