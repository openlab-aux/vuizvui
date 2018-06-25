{ lib, fetchFromGitHub, haskellPackages }:

let
  utilsSrc = fetchFromGitHub {
    owner = "Profpatsch";
    repo = "utils.hs";
    rev = "7a790aff83659bc4da8f9dc5ffb9881036b80d08";
    sha256 = "0dnsy2zcvifkl6a6l022rmxdd0mpv6qk50cd2jzgia0j90cng0ms";
  };

  nix-gen = haskellPackages.mkDerivation {
    pname = "nix-gen";
    version = "0.0.1";
    src = "${utilsSrc}/nix-gen";
    license = lib.licenses.gpl3;
    isExecutable = true;
    buildDepends = with haskellPackages; [ hnix ansi-wl-pprint protolude data-fix ];
  };

in {
  inherit nix-gen;
}
