{ pkgs, callPackage, haskellPackages, jmtpfs, libmtp, droopy, fetchFromGitHub }:

{
  display-infos = callPackage ./display-infos {};
  nman = callPackage ./nman {};
  warpspeed = callPackage ./warpspeed {
    inherit (haskellPackages) ghcWithPackages;
  };
  show-qr-code = callPackage ./show-qr-code { };

  jmtpfs = jmtpfs.override {
    libmtp = libmtp.overrideDerivation (old: {
      patches = old.patches or [] ++ [
        ./patches/mtp-jolla.patch
      ];
    });
  };

  backlight = callPackage ./backlight { inherit (pkgs.xorg) xbacklight; };

  # patched version of droopy, with javascript user-enhancement
  droopy = droopy.overrideDerivation (old: {
    src = fetchFromGitHub {
      owner = "Profpatsch";
      repo = "Droopy";
      rev = "dc63d0ac9cecd74cdff84ab9ea2a5849d6953e8a";
      sha256 = "09sms524wrnpdkhnpv9f2qbq30s8h02ljiv934g0dvmxy8571ph7";
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
