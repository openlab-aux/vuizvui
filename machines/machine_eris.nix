{ config, pkgs, lib, ... }:

{
  imports = [ ./machine_common.nix ];

  boot.loader.gummiboot.enable = true;
  boot.loader.gummiboot.timeout = 2;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelModules = [ "tp_smapi" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.tp_smapi ];
  boot.initrd.postDeviceCommands = ''
    echo noop > /sys/block/sda/queue/scheduler
  '';

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "uk";
    defaultLocale = "en_GB.UTF-8";
  };
}
