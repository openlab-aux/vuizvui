{ lib, fetchFromGitHub, haskellPackages }:

let
  utilsSrc = fetchFromGitHub {
    owner = "Profpatsch";
    repo = "utils.hs";
    rev = "1893da94a2feb58ddb7ad048b8e1691e4d0a4bc2";
    sha256 = "0x7sr61gibc2wsqj1asrsfwvd0knjh9s9x3la0mplbaxdi0vzi28";
  };
  version = "git";

  haskellDrv = { name, subfolder, deps }: haskellPackages.mkDerivation {
    pname = name;
    inherit version;
    src = "${utilsSrc}/${subfolder}";
    # TODO make utils.hs buildable from the project itself
    # src = "${/home/philip/code/haskell/utils.hs}/${subfolder}";
    license = lib.licenses.gpl3;
    isExecutable = true;
    hydraPlatforms = [ "x86_64-linux" ];
    buildDepends = deps;
  };


  nix-gen = haskellDrv {
    name = "nix-gen";
    subfolder = "nix-gen";
    deps = with haskellPackages; [ hnix ansi-wl-pprint protolude data-fix ];
  };

  until = haskellDrv {
    name = "until";
    subfolder = "until";
    deps = with haskellPackages; [ optparse-applicative data-fix time];
  };

  watch-server = haskellDrv {
    name = "watch-server";
    subfolder = "watch-server";
    deps = with haskellPackages; [ directory protolude fsnotify regex-tdfa optparse-generic ];
  };

in {
  inherit nix-gen until watch-server;
}
