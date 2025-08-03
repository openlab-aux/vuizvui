{ vuizvuiSrc ? null
, nixpkgsSrc ? <nixpkgs>
, supportedSystems ? [ "i686-linux" "x86_64-linux" ]
}:

let
  nixpkgsRevCount = nixpkgsSrc.revCount or 12345;
  nixpkgsShortRev = nixpkgsSrc.shortRev or "abcdefg";
  nixpkgsVersion = "pre${toString nixpkgsRevCount}.${nixpkgsShortRev}-vuizvui";

  nixpkgs = nixpkgsSrc;

  vuizvuiRevCount = vuizvuiSrc.revCount or 12345;
  vuizvuiShortRev = vuizvuiSrc.shortRev or "abcdefg";
  vuizvuiVersion = "pre${toString vuizvuiRevCount}.${vuizvuiShortRev}";

  # version of the nixos-unstable channel to get programs.sqlite from.
  # Use pkgs.sternenseemann.vuizvui-update-programs-sqlite to update.
  programsSqliteVersion = "25.11pre837094.94def634a204";
  programsSqliteSha256 = "1gy7brn71fmilapmg9cibri2r8yhfbxdfaiymbijqywr30syxa7d";
  programsSqlite = pkgsUpstream.fetchurl {
    name = "programs.sqlite-${programsSqliteVersion}";
    url = "https://releases.nixos.org/nixos/unstable/nixos-${programsSqliteVersion}/nixexprs.tar.xz";
    sha256 = programsSqliteSha256;
    downloadToTemp = true;
    postFetch = ''
      tar -xOJf $downloadedFile nixos-${programsSqliteVersion}/programs.sqlite > "$out"
    '';
  };

  vuizvui = let
    patchedVuizvui = (import nixpkgs {}).stdenv.mkDerivation {
      name = "vuizvui-${vuizvuiVersion}";
      inherit nixpkgsVersion;
      src = vuizvuiSrc;
      phases = [ "unpackPhase" "installPhase" ];
      installPhase = ''
        cp -r --no-preserve=ownership "${nixpkgs}/" nixpkgs
        chmod -R u+w nixpkgs
        # since we fetch nixpkgsSrc using git, we don't get programs.sqlite
        # for programs.command-not-found which is normally included in the
        # channel. Building this ourselves is not desireable as it requires
        # to build and index the whole of nixpkgs. Therefore we just inject
        # it from a nixos channel (which possibly is a different version).
        cp --no-preserve=ownership "${programsSqlite}" nixpkgs/programs.sqlite
        echo -n "$nixpkgsVersion" > nixpkgs/.version-suffix
        echo "echo '$nixpkgsVersion'" \
          > nixpkgs/nixos/modules/installer/tools/get-version-suffix
        echo -n ${nixpkgs.rev or nixpkgsShortRev} > nixpkgs/.git-revision
        echo './nixpkgs' > nixpkgs-path.nix
        cp -r . "$out"
      '';
    };
  in if vuizvuiSrc == null then ./. else patchedVuizvui;

  system = "x86_64-linux";
  pkgsUpstream = import nixpkgs { inherit system; };
  root = import vuizvui { inherit system; };

  mpath = if vuizvuiSrc == null then ./machines else "${vuizvui}/machines";

  allMachines = with pkgsUpstream.lib; let
    wrapPkgs = machine: machine.__withPkgsPath nixpkgs;
    condition = m: !(m ? __withPkgsPath);
  in mapAttrsRecursiveCond condition (const wrapPkgs) (import mpath);

  allTests = with import ./lib; getVuizvuiTests ({
    inherit system nixpkgs;
    excludeVuizvuiGames = true;
  } // pkgsUpstream.lib.optionalAttrs (vuizvuiSrc != null) {
    vuizvuiTests = "${vuizvui}/tests";
  });

  pkgs = with pkgsUpstream.lib; let
    noGames = flip removeAttrs [ "games" ];
    releaseLib = import "${nixpkgs}/pkgs/top-level/release-lib.nix" {
      inherit supportedSystems;
      packageSet = attrs: noGames (import vuizvui attrs).pkgs;
      nixpkgsArgs.config = {
        allowUnfree = false;
        inHydra = true;
        allowBroken = true;
      };
    };

    packagePlatforms = mapAttrs (name: value: let
      brokenOr = if value.meta.broken or false then const [] else id;
      platforms = value.meta.hydraPlatforms or (value.meta.platforms or []);
      isRecursive = value.recurseForDerivations or false
                 || value.recurseForRelease or false;
      result = if isDerivation value then brokenOr platforms
               else if isRecursive then packagePlatforms value
               else [];
      tried = builtins.tryEval result;
    in if tried.success then tried.value else []);

  in with releaseLib; mapTestOn (packagePlatforms releaseLib.pkgs);

in with pkgsUpstream.lib; with builtins; {

  machines = let
    # We need to expose all the real builds within vuizvui.lazyPackages to make
    # sure they don't get garbage collected on the Hydra instance.
    wrapLazy = machine: pkgsUpstream.runCommandLocal machine.build.name {
      fakeRuntimeDeps = machine.eval.config.vuizvui.lazyPackages;
      product = machine.build;
    } ''
      mkdir -p "$out/nix-support"
      echo "$product" > "$out/nix-support/fake-runtime-dependencies"
      for i in $fakeRuntimeDeps; do
        echo "$i" >> "$out/nix-support/fake-runtime-dependencies"
      done
    '';
  in mapAttrsRecursiveCond (m: !(m ? eval)) (const wrapLazy) allMachines;

  isoImages = let
    buildIso = attrs: let
      name = attrs.iso.config.networking.hostName;
      cond = attrs.iso.config.vuizvui.createISO;
    in if !cond then {} else pkgsUpstream.runCommandLocal "vuizvui-iso-${name}" {
      meta.description = "Live CD/USB stick of ${name}";
      iso = attrs.iso.config.system.build.isoImage;
      passthru.config = attrs.iso.config;
    } ''
      mkdir -p "$out/nix-support"
      echo "file iso" $iso/iso/*.iso* \
        >> "$out/nix-support/hydra-build-products"
    '';
  in mapAttrsRecursiveCond (m: !(m ? iso)) (const buildIso) allMachines;

  tests = let
    machineList = collect (m: m ? eval) allMachines;
    activatedTests = unique (concatMap (machine:
      machine.eval.config.vuizvui.requiresTests
    ) machineList);
    mkTest = path: setAttrByPath path (getAttrFromPath path allTests);
  in fold recursiveUpdate {} (map mkTest activatedTests) // {
    inherit (allTests) vuizvui;
  };

  inherit pkgs;

  channels = let
    mkChannel = attrs: root.pkgs.mkChannel (rec {
      name = "vuizvui-channel-${attrs.name or "generic"}-${vuizvuiVersion}";
      src = vuizvui;
      patchPhase = ''
        touch .update-on-nixos-rebuild
      '';
    } // removeAttrs attrs [ "name" ]);

    gatherTests = active: map (path: getAttrFromPath path allTests) active;

  in {
    generic = mkChannel {
      constituents = concatMap (collect isDerivation) [
        allTests.vuizvui pkgs
      ];
    };

    machines = mapAttrsRecursiveCond (m: !(m ? eval)) (path: attrs: mkChannel {
      name = "machine-${last path}";
      constituents = singleton attrs.eval.config.system.build.toplevel
                  ++ gatherTests attrs.eval.config.vuizvui.requiresTests;
    }) allMachines;
  };

  manual = let
    modules = import "${nixpkgs}/nixos/lib/eval-config.nix" {
      modules =
        import "${vuizvui}/modules/module-list.nix"
        ++ [
          {
            config = {
              _module.check = false;
              nixpkgs = { inherit system; };
            };
          }
        ];
    };

    optionsDoc = pkgsUpstream.nixosOptionsDoc {
      documentType = "none";
      warningsAreErrors = false;
      options = {
        inherit (modules.options) vuizvui;
      };
    };

    optionsJSON = "${optionsDoc.optionsJSON}/share/doc/nixos/options.json";
  in
  pkgsUpstream.runCommand "vuizvui-options" {
    # Interestingly, optionsJSON doesn't create references to vuizvuiSrc
    allowedReferences = [ "out" ];
    nativeBuildInputs = [ pkgsUpstream.nixos-render-docs ];
  } ''
    dest="$out/share/doc/vuizvui"
    mkdir -p "$dest"

    cp "${nixpkgs + "/doc/style.css"}" "$dest/style.css"
    cp "${nixpkgs + "/doc/anchor.min.js"}" "$dest/anchor.min.js"
    cp "${nixpkgs + "/doc/anchor-use.js"}" "$dest/anchor-use.js"
    cp -r ${pkgsUpstream.documentation-highlighter} "$dest/highlightjs"

    cp -r "${./doc}" doc
    chmod -R +w doc

    substituteInPlace doc/index.md \
      --replace-fail @VUIZVUI_VERSION@ "${vuizvuiVersion}"
    substituteInPlace doc/options.md \
      --replace-fail @VUIZVUI_OPTIONS_JSON@ "${optionsJSON}"

    # N. B. nixos-render-docs always assumes we are building a NixOS manual.
    # As such, source code references will link to https://github.com/NixOS/nixpkgs/…
    # if it doesn't know an absolute path to the declaring module. This should
    # never be the case for us, but --revision (as in nixpkgs) is a required flag.
    nixos-render-docs -j $NIX_BUILD_CORES manual html \
      --revision bogus \
      --manpage-urls ${nixpkgs + "/doc/manpage-urls.json"} \
      --stylesheet style.css \
      --stylesheet highlightjs/mono-blue.css \
      --script ./highlightjs/highlight.pack.js \
      --script ./highlightjs/loader.js \
      --script ./anchor.min.js \
      --script ./anchor-use.js \
      doc/index.md \
      "$dest/index.html"

    mkdir -p "$out/nix-support"
    echo "doc manual $dest" > "$out/nix-support/hydra-build-products"
  '';
}
