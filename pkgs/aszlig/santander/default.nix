{ stdenv, fetchurl, fetchgit, fetchFromBitbucket
, runCommand, writeScript, writeScriptBin, writeText
, xvfb_run, xdotool, coreutils, wineMinimal, pipelight, dwb-unwrapped, pcsclite
}:

let
  name = "SecurityPluginHBCIChipcard";
  version = "2.9.8.0";
  dllName = "NP_${name}.dll";

  pluginInstaller = fetchurl {
    url = "https://service.santanderbank.de/special/banking/files/"
        + "SecurityPluginHBCIChipcard-${version}-Installer.exe";
    sha256 = "0xnfb730mwxdx83dnqyplp4bxwx6g01wc87xa4dl1spxia9kjmmh";
  };

  patchedWine = let
    libpcsclite = "${pcsclite}/lib/libpcsclite.so";
  in (wineMinimal.override {
    wineBuild = "wine32";
    wineRelease = "staging";
  }).overrideDerivation (drv: {
    scard4wine = fetchgit {
      url = "git://git.code.sf.net/p/scard4wine/code";
      rev = "c14c02c80bf1f2bb4cedd1f53a3a2ab9c48bed76";
      sha256 = "0ffmbl9mdnaih4h3ggpnzqbih3kgbwl3wv6j1ag5s4czn8gcpdq3";
    };

    prePatch = (drv.prePatch or "") + ''
      cp -t dlls/winscard "$scard4wine/src/"*
      sed -i -re 's,"libpcsclite\.so(\.[0-9]+)*","${libpcsclite}",' \
        dlls/winscard/winscard.c
    '';

    patches = (drv.patches or []) ++ [ ./winscard.patch ];

    postPatch = (drv.postPatch or "") + ''
      sed -i -e '/not owned by you/d' libs/wine/config.c
      # Modified patch from https://bugs.winehq.org/show_bug.cgi?id=22450
      patch -p1 < "${./wine-no-unixfs.patch}"
    '';
  });

  installPath = [ "Program Files" "ppi" "SecurityPluginHBCIChipcard" ];

  scard4wine = stdenv.mkDerivation rec {
    name = "scard4wine-${version}";
    version = "1.2.0-2016-06-05";

    src = fetchgit {
      url = "git://git.code.sf.net/p/scard4wine/code";
      rev = "c14c02c80bf1f2bb4cedd1f53a3a2ab9c48bed76";
      sha256 = "0ffmbl9mdnaih4h3ggpnzqbih3kgbwl3wv6j1ag5s4czn8gcpdq3";
    };
  };

  winePrefix = runCommand "santander-wineprefix" {
    installPath = stdenv.lib.concatStringsSep "/" (installPath ++ [ dllName ]);
  } ''
    export WINEPREFIX="$out"
    export WINEDLLOVERRIDES="mscoree,mshtml="
    mkdir -p "$out"
    ${patchedWine}/bin/wine wineboot.exe
    ${xvfb_run}/bin/xvfb-run "${writeScript "install-santander-wine" ''
      ${patchedWine}/bin/wine "${pluginInstaller}" &
      while [ "$(jobs -r | wc -l)" -gt 0 ]; do
        ${xdotool}/bin/xdotool \
          search --sync --onlyvisible \
          --name 'Security-Plugin-HBCI-Chipcard ${version}' \
          key Return &> /dev/null || :
        sleep 1
      done
      wait
    ''}"
    if [ ! -e "$out/drive_c/$installPath" ]; then
      echo "Unable to find plugin in $installPath." >&2
      exit 1
    fi
    ln -sf -T "${builtins.storeDir}" "$WINEPREFIX/dosdevices/z:"
    echo disable > "$WINEPREFIX/.update-timestamp"
  '';

  pluginConfig = {
    winePath = "$share/wine";
    inherit winePrefix dllName;
    wineArch = "win32";
    pluginLoaderPath = "$share/pluginloader.exe";
    dllPath = "c:\\${stdenv.lib.concatStringsSep "\\" installPath}";
  };

  pipelightConfigFile = let
    mkVal = val: if val == true then "true"
            else if val == false then "false"
            else toString val;
    mkCfgLine = key: val: "# ${key} = ${mkVal val}";
  in with stdenv.lib; writeText "pipelight-santander.config" ''
    # ---BEGIN CONFIG---
    ${concatStringsSep "\n" (mapAttrsToList mkCfgLine pluginConfig)}
    # ---END CONFIG---
  '';

  finalPlugin = runCommand "santander-plugin" {
    pipelight = (pipelight.override {
      wineStaging = patchedWine;
    }).overrideDerivation (drv: {
      src = fetchFromBitbucket {
        repo = "pipelight";
        owner = "mmueller2012";
        rev = "181bab804f80b99cb46f63f9ed36e4fdf12ca319";
        sha256 = "0ydivpxayzs5aklf0x5vl5bl4issz10k7zl3cv76649kxxhxkh1z";
      };

      patches = [ ./pipelight.patch ];

      postPatch = (drv.postPatch or "") + ''
        sed -i -e '/static \+bool \+openConfig.*{$/,/}/ {
          /getConfigNameFromLibrary/a \
            configFile.open("${pipelightConfigFile}"); \
            if (configFile.is_open()) return true;
        }' src/linux/libpipelight/configloader.c
      '';

      # We don't want or have share/pipelight/install-dependency!
      preFixup = null;
    });
  } ''
    install -vD "$pipelight/lib/pipelight/libpipelight.so" \
      "$out/lib/pipelight/libpipelight-santander.so"
  '';

  # Allow to use dwb for now until we have a better solution.
  dwb = dwb-unwrapped.override {
    inherit (import (import ../../../nixpkgs-path.nix) {
      inherit (stdenv) system;
      config = {
        permittedInsecurePackages = [ "webkitgtk-2.4.11" ];
      };
    }) webkitgtk2;
  };

  inherit (stdenv.lib) escapeShellArg;

in writeScriptBin "santander" ''
  #!${stdenv.shell}
  if tmpdir="$("${coreutils}/bin/mktemp" -d)"; then
    trap "rm -rf '$tmpdir'" EXIT
    export HOME="$tmpdir"
    export MOZ_PLUGIN_PATH=${escapeShellArg "${finalPlugin}/lib/pipelight"}
    "${dwb}/bin/dwb" -t https://karte.santanderbank.de/
    exit $?
  else
    echo "Unable to create temporary profile directory." >&2
    exit 1
  fi
''
