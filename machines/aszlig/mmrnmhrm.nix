{ pkgs, lib, ... }:

with lib;

{
  vuizvui.user.aszlig.profiles.workstation.enable = true;

  nix.maxJobs = 2;

  boot = {
    loader.grub.devices = map (i: "/dev/disk/by-id/${i}") [
      "ata-WDC_WD10EZEX-00BN5A0_WD-WCC3F5756955"
      "ata-WDC_WD10EZEX-00BN5A0_WD-WCC3F5790537"
    ];
  };

  vuizvui.user.aszlig.system.kernel.enable = true;

  networking.hostName = "mmrnmhrm";

  fileSystems = {
    "/" = {
      label = "root";
      fsType = "btrfs";
      options = [ "autodefrag" "space_cache" "compress=lzo" "noatime" ];
    };
  };

  swapDevices = [
    { label = "swap1"; }
    { label = "swap2"; }
  ];

  services.synergy.server.enable = true;
  services.synergy.server.configFile = pkgs.writeText "synergy.conf" ''
    section: screens
      dnyarri:
      mmrnmhrm:
      tishtushi:
    end

    section: links
      mmrnmhrm:
        left = dnyarri
        right = dnyarri
      dnyarri:
        right = mmrnmhrm
        left = mmrnmhrm
    end

    section: options
      keystroke(Super+F1) = switchToScreen(dnyarri)
      keystroke(Super+F2) = switchToScreen(mmrnmhrm)
      keystroke(Super+F3) = switchToScreen(tishtushi)
    end
  '';

  services.kmscon.enable = true;

  systemd.services."synergy-server".serviceConfig.CPUSchedulingPolicy = "rr";
  systemd.services."synergy-server".serviceConfig.CPUSchedulingPriority = 50;

  services.xserver.videoDrivers = [ "nouveau" ];
  services.xserver.xrandrHeads = [ "DVI-I-1" "VGA-1" ];

  vuizvui.user.aszlig.services.i3.workspaces."1" = {
    label = "XMPP";
    assign = singleton { class = "^(?:Tkabber|Gajim)\$"; };
  };

  vuizvui.user.aszlig.services.i3.workspaces."3" = {
    label = "Chromium";
    assign = singleton { class = "^Chromium(?:-browser)?\$"; };
  };
}
