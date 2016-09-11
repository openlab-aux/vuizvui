{ lib, fetchFromGitHub, writeScriptBin
, haskellPackages, mpg321 }:

let
  repo = fetchFromGitHub {
    owner = "openlab-aux";
    repo = "stackenblocken";
    rev = "labpingbot";
    sha256 = "1x319sbkk8hl3lad2zapkdv6ihqqsl8f5l0a2n9fvppcm5c7pz0d";
 };

 bot = haskellPackages.callPackage "${repo}/stackenblocken.nix" {};
 jingle = "${repo}/stackenblocken_jingle.mp3";

 script = ''
   #!/bin/sh

   # kill everything on SIGINT
   trap exit SIGINT
   # also running background processes
   trap "kill 0" EXIT

   for i in $(seq 2); do
     echo "starting .labping bot"
     ${lib.getBin bot}/bin/stackenblocken &

     echo "DOING STACKENBLOCKEN"
     ${lib.getBin mpg321}/bin/mpg321 --gain 40 -q ${jingle}
   done
 '';


in
  writeScriptBin "stackenblocken" script

