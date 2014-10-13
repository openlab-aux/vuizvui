{ pkgs, ... }:

with pkgs.lib;
with import ../lib;

{
  imports = singleton ../common-workstation.nix;

  nix.maxJobs = 2;

  boot = let
    linuxVuizvui = pkgs.buildLinux {
      inherit (pkgs.kernelSourceVuizvui) version src;

      kernelPatches = singleton pkgs.vuizvuiKernelPatches.bfqsched;
      configfile = generateKConf (import ./mmrnmhrm-kconf.nix);
      allowImportFromDerivation = true;
    };
  in rec {
    kernelPackages = pkgs.recurseIntoAttrs
      (pkgs.linuxPackagesFor linuxVuizvui kernelPackages);

    loader.grub.devices = map (i: "/dev/disk/by-id/${i}") [
      "ata-WDC_WD10EZEX-00BN5A0_WD-WCC3F5756955"
      "ata-WDC_WD10EZEX-00BN5A0_WD-WCC3F5790537"
    ];
  };

  networking.hostName = "mmrnmhrm";

  fileSystems = {
    "/" = {
      label = "root";
      fsType = "btrfs";
      options = concatStringsSep "," [
        "autodefrag"
        "space_cache"
        "compress=lzo"
        "noatime"
      ];
    };
  };

  swapDevices = [
    { label = "swap1"; }
    { label = "swap2"; }
  ];

  services.synergy.server.enable = true;
  services.synergy.server.configFile = ../cfgfiles/synergy.conf;

  services.xserver.videoDrivers = [ "nouveau" ];
  services.xserver.xrandrHeads = [ "DVI-I-1" "VGA-1" ];

  vuizvui.i3.workspaces."1" = {
    label = "XMPP";
    assign = singleton { class = "^(?:Tkabber|Gajim)\$"; };
  };

  vuizvui.i3.workspaces."3" = {
    label = "Chromium";
    assign = singleton { class = "^Chromium(?:-browser)?\$"; };
  };
}
