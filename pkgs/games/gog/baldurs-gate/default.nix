{ stdenv, lib, fetchFromGitHub, autoreconfHook
, buildGame, makeWrapper, fetchGog, openal, libGL
}:

let
  patchelf = stdenv.mkDerivation {
    pname = "patchelf";
    version = "2020-06-23";

    src = fetchFromGitHub {
      owner = "NixOS";
      repo = "patchelf";
      rev = "e61654be0acf4454ab070a8c8e1345aa7d5b3d74";
      sha256 = "1bfr6303i0hii8s86kj2j8dhi97abgancb08xpvfx2dh6608kcr0";
    };

    patches = [ ./patchelf-remove-verneed.patch ];
    nativeBuildInputs = [ autoreconfHook ];
  };

  mkBaldursGate = { pname, version, src, executable }: buildGame rec {
    name = "${pname}-${version}";
    inherit pname version src;

    nativeBuildInputs = [ makeWrapper ];
    buildInputs = [ openal libGL ];
    execName = executable + lib.optionalString stdenv.is64bit "64";

    # The game uses libjingle for NAT traversal only, so let's remove the
    # XMPP-related libraries since all we should need here is STUN/TURN
    # support.
    buildPhase = ''
      ${patchelf}/bin/patchelf \
        --remove-needed libcrypto.so.1.0.0 \
        --remove-needed libssl.so.1.0.0 \
        --remove-needed libexpat.so.1 \
        "$execName"
    '';

    installPhase = ''
      install -vD "$execName" "$out/libexec/$pname/$pname"
      install -vD -m 0644 engine.lua "$out/share/$pname/engine.lua"
      install -vD -m 0644 chitin.key "$out/share/$pname/chitin.key"
      cp -rt "$out/share/$pname" data lang movies music scripts
      makeWrapper "$out/libexec/$pname/$pname" "$out/bin/$pname" \
        --run "cd '$out/share/$pname'"
    '';

    # Allow access to both data dirs to allow export/import of characters.
    sandbox.paths.required = [
      "$XDG_DATA_HOME/Baldur's Gate - Enhanced Edition"
      "$XDG_DATA_HOME/Baldur's Gate II - Enhanced Edition"
    ];
  };

in {
  bg1ee = mkBaldursGate {
    pname = "baldurs-gate";
    version = "2.5.23121";
    executable = "BaldursGate";

    src = fetchGog {
      productId = 1207666353;
      sha256 = "0r8d4pc7rszkgw0wwfvzvng48hc0915qz6vw3hpkhrffccsl9bc0";
    };
  };

  bg2ee = mkBaldursGate {
    pname = "baldurs-gate-2";
    version = "2.5.21851";
    executable = "BaldursGateII";

    src = fetchGog {
      productId = 1207666373;
      sha256 = "0l4rbc1zbhasnkbfcspbjb8vwsw574ld2aw6x9gk9bdbr8rz0fik";
    };
  };
}
