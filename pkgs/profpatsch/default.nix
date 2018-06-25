{ pkgs, callPackage, haskellPackages, droopy, fetchFromGitHub }:

{
  backlight = callPackage ./backlight { inherit (pkgs.xorg) xbacklight; };
  display-infos = callPackage ./display-infos {};
  nix-http-serve = callPackage ./nix-http-serve {};
  nman = callPackage ./nman {};
  show-qr-code = callPackage ./show-qr-code {};
  warpspeed = callPackage ./warpspeed {
    inherit (haskellPackages) ghcWithPackages;
  };

  inherit (callPackage ./utils-hs {})
    nix-gen;

  # patched version of droopy, with javascript user-enhancement
  droopy = droopy.overrideDerivation (old: {
    src = fetchFromGitHub {
      owner = "Profpatsch";
      repo = "Droopy";
      rev = "55c60c612b913f9fbce9fceebbcb3a332152f1a4";
      sha256 = "0jcazj9gkdf4k7vsi33dpw9myyy02gjihwsy36dfqq4bas312cq1";
    };
    installPhase = old.installPhase or "" + ''
      mkdir -p $out/share/droopy
      cp -r $src/static $out/share/droopy
    '';
    makeWrapperArgs = old.makeWrapperArgs or [] ++ [
      "--set DROOPY_STATIC \"$out/share/droopy/static\""
    ];

  });
}
