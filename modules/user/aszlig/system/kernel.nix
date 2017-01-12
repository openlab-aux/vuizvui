{ config, pkgs, lib, ... }:

{
  options.vuizvui.user.aszlig.system.kernel = {
    enable = lib.mkEnableOption "aszlig's custom kernel";
  };

  config = lib.mkIf config.vuizvui.user.aszlig.system.kernel.enable {
    boot = {
      kernelPatches = lib.singleton {
        name = "bfq-v8r7";
        patch = pkgs.fetchpatch {
          name = "bfq-v8r7.patch";
          url = "https://github.com/linusw/linux-bfq/compare/"
              + "238d1d0f79f619d75c2cc741d6770fb0986aef24..."
              + "eaa6f84b3c10985b01a1d7ff1a77fb5f43df714d.patch";
          sha256 = "1ir9m5smd82maipy7jjs4vmfsz18qz51gz3zy9gmmiaznm0i9llx";
        };
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
