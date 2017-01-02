{ pkgs, lib, ... }:

{
  vuizvui.user.aszlig.profiles.workstation.enable = true;

  nix.maxJobs = 8;

  boot = {
    initrd = {
      mdadmConf = ''
        ARRAY /dev/md0 metadata=1.2 UUID=f5e9de04:89efc509:4e184fcc:166b0b67
        ARRAY /dev/md1 metadata=0.90 UUID=b85aa8be:cea0faf2:7abcbee8:eeae037b
      '';
      luks.devices = [
        { name = "system_crypt";
          device = "/dev/md1";
          preLVM = true;
        }
      ];
    };

    loader.grub.devices = [
      "/dev/disk/by-id/ata-ST31500541AS_5XW0AMNH"
      "/dev/disk/by-id/ata-ST31500541AS_6XW0M217"
    ];
  };

  environment.systemPackages = [ pkgs.paperwork ];

  # This is very ugly and I really want to avoid non-free packages on all
  # of my workstations. But right now I need to get rid of useless paper.
  nixpkgs.config = {
    allowUnfreePredicate = pkg: let
      inherit (builtins.parseDrvName pkg.name) name;
    in lib.hasSuffix "-hplip" name && lib.hasPrefix "python" name;
    packageOverrides = super: {
      hplip = super.hplip.override { withPlugin = true; };
    };
  };

  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.hplip ];

  vuizvui.user.aszlig.system.kernel.enable = true;
  hardware.enableAllFirmware = true;

  networking.hostName = "dnyarri";

  fileSystems = {
    "/boot" = {
      label = "boot";
      fsType = "ext2";
    };
    "/" = {
      device = "/dev/shofixti/root";
      fsType = "xfs";
    };
  };

  powerManagement.powerUpCommands = ''
    ${pkgs.hdparm}/sbin/hdparm -B 255 /dev/disk/by-id/ata-ST31500541AS_5XW0AMNH
    ${pkgs.hdparm}/sbin/hdparm -B 255 /dev/disk/by-id/ata-ST31500541AS_6XW0M217
  '';

  swapDevices = lib.singleton {
    device = "/dev/shofixti/swap";
  };

  users.users.aszlig.extraGroups = [
    "scanner"
    # TODO: Try to avoid this, but as there is only a single user using audio
    # on this machine, it's okay for now. But remember that this will break
    # heavily, should there be another user accessing the audio devices.
    "audio"
  ];

  services.synergy.client.enable = true;
  services.synergy.client.serverAddress = "mmrnmhrm";

  services.kmscon.enable = true;

  systemd.services."synergy-client".serviceConfig.CPUSchedulingPolicy = "rr";
  systemd.services."synergy-client".serviceConfig.CPUSchedulingPriority = 50;

  services.xserver.videoDrivers = [ "ati" ];
  services.xserver.xrandrHeads = [ "HDMI-0" "DVI-0" ];

  vuizvui.user.aszlig.services.i3.reverseHeads = true;
  vuizvui.user.aszlig.services.i3.workspaces."6" = {
    label = "Chromium";
    assign = lib.singleton { class = "^Chromium(?:-browser)?\$"; };
  };
}
