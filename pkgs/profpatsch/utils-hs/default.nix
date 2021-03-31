{ lib, fetchFromGitHub, haskellPackages, haskell }:

let
  utilsSrc = fetchFromGitHub {
    owner = "Profpatsch";
    repo = "utils.hs";
    rev = "f53264978042d8041831a3ac3766aa1dfdc60b57";
    sha256 = "18mxcbi6cv81y3jvp9lrfp793vayvd9llpkcjh5szb8vi1kizrgy";
  };
  version = "git";

  # TODO: make it possible to override the hps fixpoint again
  # without removing the overrides in here
  hps =
    let hlib = haskell.lib; in
    haskellPackages.override {
      overrides = (hself: hsuper: {

        # shell stub
        shellFor = f: # self -> { buildDepends, buildTools }
          let args = f hself;
          in hsuper.mkDerivation {
            pname = "pkg-env";
            src = "/dev/null";
            version = "none";
            license = "none";
            inherit (args) buildDepends;
            buildTools = with hself; [
              ghcid
              cabal-install
              hpack
              (hoogleLocal {
                packages = args.buildDepends;
              })
            ] ++ args.buildTools or [];
          };

        # hoogleLocal should never use the builders
        hoogleLocal = args: (hsuper.hoogleLocal args).overrideAttrs (_: {
          preferLocalBuild = true;
          allowSubstitutes = false;
        });

        these = hlib.doJailbreak hsuper.these;
      });
    };

  haskellDrv = { name, subfolder, deps }: hps.mkDerivation {
    pname = name;
    inherit version;
    src = "${utilsSrc}/${subfolder}";
    # TODO make utils.hs buildable from the project itself
    # src = "${/home/philip/code/haskell/utils.hs}/${subfolder}";
    license = lib.licenses.gpl3;
    isExecutable = true;
    hydraPlatforms = [ "x86_64-linux" ];
    buildDepends = deps;

    # justStaticExecutables
    enableSharedExecutables = false;
    enableLibraryProfiling = false;
    isLibrary = false;
    doHaddock = false;
    postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
  };

  until = haskellDrv {
    name = "until";
    subfolder = "until";
    deps = with hps; [ optparse-applicative data-fix time];
  };

  watch-server = haskellDrv {
    name = "watch-server";
    subfolder = "watch-server";
    deps = with hps; [ directory protolude fsnotify regex-tdfa optparse-generic ];
  };

in {
  inherit until watch-server;
  haskellPackages = hps;
}
