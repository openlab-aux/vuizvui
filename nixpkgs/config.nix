{
  pulseaudio = true;
  chromium.jre = true;
  firefox.jre = true;

  packageOverrides = pkgs: let
    mainOverrides = import ../overrides pkgs;
    envs = import ../envs (pkgs // mainOverrides);
  in mainOverrides // envs;
}
