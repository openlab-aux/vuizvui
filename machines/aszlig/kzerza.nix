{ pkgs, lib, ... }:

let
  rootUUID = "ad41f848-d14a-4a89-9d04-3e48bd73dc5c";
  diskID = "usb-0000_Removable_Drive_23372707080836980013-0:0";
in {
  vuizvui.user.aszlig.profiles.base.enable = true;
  vuizvui.createISO = true;

  services.xserver.enable = lib.mkForce false;

  services.gpm.enable = true;
  services.gpm.protocol = "exps2";

  boot = {
    kernelParams = lib.singleton "consoleblank=0";
    initrd.kernelModules = [ "fbcon" "usb_storage" ];
    loader.grub.device = "/dev/disk/by-id/${diskID}";
  };

  networking.hostName = "kzerza";

  fileSystems."/".device = "/dev/disk/by-uuid/${rootUUID}";
  fileSystems."/".fsType = "btrfs";
  fileSystems."/".options = [
    "ssd"
    "space_cache"
    "compress-force=zlib"
    "noatime"
  ];

  services.udev.extraRules = ''
    SUBSYSTEM=="usb*|tty", ACTION=="add|change", ATTRS{idVendor}=="0403", \
      ATTRS{idProduct}=="6001", OWNER="grandpa"
  '';

  fileSystems."/tmp".device = "none";
  fileSystems."/tmp".fsType = "tmpfs";
  fileSystems."/tmp".options = [ "nosuid" "nodev" "relatime" ];

  users.groups.grandpa.gid = 666;
  users.users.grandpa = {
    uid = 666;
    description = "GrandPA User";
    group = "grandpa";
    createHome = true;
  };

  systemd.services.grandpa = {
    description = "GrandPA Lighting Controller";
    wantedBy = [ "multi-user.target" ];
    preStart = "${pkgs.kbd}/bin/chvt 7";
    serviceConfig = {
      Type = "idle";
      ExecStart = "${pkgs.vuizvui.aszlig.grandpa}/bin/grandpa";
      ExecStopPost = "${pkgs.systemd}/bin/systemctl poweroff";
      Restart = "on-failure";
      StandardInput = "tty";
      StandardOutput = "tty";
      TTYPath = "/dev/tty7";
      TTYVTDisallocate = true;
      User = "grandpa";
      Group = "grandpa";
      PermissionsStartOnly = true;
      PrivateTmp = true;
      PrivateNetwork = true;
    };
  };
}
