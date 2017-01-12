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
              + "eaa6f84b3c10985b01a1d7ff1a77fb5f43df714d.diff";
          postFetch = ''
            "${pkgs.patchutils}/bin/filterdiff" -x '[ab]/Makefile' "$out" > tmp
            mv tmp "$out"
          '';
          sha256 = "1q108w6w5l4qdnicp01c4kb4rdgs2q34vh0gb5436hsj5diai69q";
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
