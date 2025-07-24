{ config, pkgs, lib, ... }:

{
  options.vuizvui.system.kernel.useBleedingEdge = lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = ''
      Whether to always use the latest kernel, even if it's still a release
      candidate version.
    '';
  };

  config = lib.mkIf config.vuizvui.system.kernel.useBleedingEdge {
    boot.kernelPackages = let
      inherit (lib) take splitString replaceStrings;
      inherit (pkgs) linux_latest linux_testing;
      dotizeVer = replaceStrings ["-"] ["."];
      trimVer = ver: take 2 (splitString "." (dotizeVer ver));
      tooOld = trimVer linux_latest.version == trimVer linux_testing.version;
      kernel = if tooOld then linux_latest else linux_testing;
    in pkgs.linuxPackagesFor kernel;
  };
}
