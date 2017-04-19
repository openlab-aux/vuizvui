{ config, pkgs, lib, ... }:

{
  options.vuizvui.user.aszlig.system.kernel = {
    enable = lib.mkEnableOption "aszlig's custom kernel";
  };

  config = lib.mkIf config.vuizvui.user.aszlig.system.kernel.enable {
    boot = {
      kernelPatches = lib.singleton {
        name = "bfq-v8r7";
        patch = ./bfq.patch;
        extraConfig = (pkgs.vuizvui.kernel.bfqsched.extraConfig or "") + ''
          IOSCHED_BFQ y
          DEFAULT_BFQ y
          DEFAULT_CFQ n
          DEFAULT_IOSCHED bfq
        '';
      };

      kernelPackages = let
        inherit (lib) take splitString replaceStrings;
        inherit (pkgs) linux_latest linux_testing;
        dotizeVer = replaceStrings ["-"] ["."];
        trimVer = ver: take 2 (splitString "." (dotizeVer ver));
        tooOld = trimVer linux_latest.version == trimVer linux_testing.version;
        kernel = if tooOld then linux_latest else linux_testing;
      in pkgs.linuxPackagesFor kernel;
    };
  };
}
