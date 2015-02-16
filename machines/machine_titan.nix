{ config, pkgs, lib, ... }:

{
  imports = [ ./machine_common.nix ];

  boot.loader.gummiboot.enable = true;
  boot.loader.gummiboot.timeout = 2;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = [ "elevator=noop" ];

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "uk";
    defaultLocale = "en_GB.UTF-8";
  };
}
