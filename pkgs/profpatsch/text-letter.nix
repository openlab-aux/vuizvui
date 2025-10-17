{ pkgs, rust-deps, writeRustSimple, writeExecline, profpatsch }:

let
  bins = profpatsch.utils.getBins pkgs.coreutils [ "date" "cat" ]
      // profpatsch.utils.getBins pkgs.paps [ "paps" ]
      // profpatsch.utils.getBins pkgs.ghostscript [ "ps2pdf" ];

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
