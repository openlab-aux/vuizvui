{ stdenv, pkgs, fetchFromGitHub }:

let
  src = fetchFromGitHub {
    owner = "Profpatsch";
    repo = "nix-http-serve";
    rev = "f1f188da4a78c3d359cc1a92663d82ee8c6acd2f";
    sha256 = "12qfpzhij0si2p5p8d1iri1iz2kv2bn3jsvqbw32x9gfl728i1bi";
  };

in (pkgs.callPackage src {}).nix-http-serve
