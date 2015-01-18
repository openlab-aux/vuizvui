{ config, pkgs, lib, ... }:

{
  boot.loader.grub = {
    enable = true;
    version = 2;
    devices = [
      "/dev/disk/by-id/ata-ST31500541AS_6XW0NK21"
      "/dev/disk/by-id/ata-ST31500541AS_6XW0P0CW"
    ];
  };

  i18n = {
    consoleFont = "lat9w16";
    consoleKeyMap = "dvorak";
    defaultLocale = "en_US.UTF-8";
  };

  powerManagement.powerUpCommands = ''
    ${pkgs.hdparm}/sbin/hdparm -B 255 \
  /dev/disk/by-id/ata-ST31500541AS_6XW0NK21
    ${pkgs.hdparm}/sbin/hdparm -B 255 \
  /dev/disk/by-id/ata-ST31500541AS_6XW0P0CW
  '';
}
