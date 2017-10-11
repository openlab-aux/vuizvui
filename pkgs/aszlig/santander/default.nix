{ stdenv, fetchurl, fetchgit, runCommand, p7zip, jq, wineMinimal, pcsclite }:

let
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

in stdenv.mkDerivation rec {
  name = "TRAVIC-Sign-${version}";
  version = "3.1.3.0";

  src = fetchurl {
    url = "https://service.santanderbank.de/special/banking/files/"
        + "${name}-Installer.exe";
    sha256 = "19a14av3bg6i4iy5q5pa737cwxznqji0lcrapxw0q6qb8rs1rhs7";
  };

  extensionId = "ilpoejcegjjlgpobjkpjmddkbdkdndaj";

  buildInputs = [ p7zip jq ];

  unpackCmd = "7z x -y -otavic-sign $curSrc";

  phases = [ "unpackPhase" "patchPhase" "installPhase" ];

  postPatch = ''
    jq '.allowed_origins = [
      "chrome-extension://'"$extensionId"'/"
    ] | .path = "'"$out/share/libexec/travic-sign"'"
      | del(.allowed_extensions)' manifest-firefox.json > host.json

    7z x -y -oextension FirefoxExtension.xpi
    jq '.content_scripts[].matches = ["https://karte.santanderbank.de/*"] | {
      # All the object attributes that we want to have (nothing more):
      background, web_accessible_resources, content_scripts, page_action,
      permissions, author, version, description, name, manifest_version
    }' extension/manifest.json > new_manifest.json
    mv new_manifest.json extension/manifest.json
    (cd extension && 7z a -tzip ../travic-sign.crx *)
  '';

  winePrefix = runCommand "empty-wineprefix" {
    buildInputs = [ patchedWine ];
  } ''
    export WINEPREFIX="$out"
    mkdir -p "$out"
    wine wineboot.exe
  '';

  installPhase = ''
    libexec="$out/share/libexec/travic-sign"

    install -vD -m 0644 TRAVIC-Sign-Service.exe "$libexec/service.exe"
    install -vD -m 0644 host.json \
      "$out/etc/chromium/native-messaging-hosts/travic-sign.json"
    install -vD -m 0644 travic-sign.crx \
      "$out/share/chromium/extensions/$extensionId.crx"

    cat > "$libexec/travic-sign" <<EOF
    #!${stdenv.shell}
    export WINEPREFIX="$winePrefix"
    exec ${patchedWine}/bin/wine "$libexec/service.exe"
    EOF
    chmod +x "$libexec/travic-sign"
  '';
}
