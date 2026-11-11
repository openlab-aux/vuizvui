{ pkgs, ... }:

{
  config = {
    services.usbmuxd.enable = true;
    environment.systemPackages = [
      pkgs.ifuse
      pkgs.libimobiledevice
    ];
  };
}
