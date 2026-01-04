{ pkgs }:

pkgs.droopy.overrideDerivation (old: {
  src = pkgs.fetchFromGitHub {
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
})
