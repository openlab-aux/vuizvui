{ config, pkgs, utils, lib, ... }:

let
  mkDevice = category: num: uuid: {
    name = "dnyarri-${category}-crypt-${toString num}";
    value.device = "/dev/disk/by-uuid/${uuid}";
  };

  cryptDevices = {
    root = lib.imap (mkDevice "root") [
      "36260b1f-b403-477f-ab0e-505061c4e9d8"
      "3d5d71fa-ca2a-4144-a656-c68378cd2128"
    ];
    swap = lib.imap (mkDevice "swap") [
      "537b8b6b-0f03-4b2a-b0bb-6ebf18f7d9a0"
      "82d5a52d-1661-474d-859d-85c7d400d4b5"
    ];
  };

in {
  vuizvui.user.aszlig.profiles.workstation.enable = true;

  nix.maxJobs = 24;

  boot = {
    loader.systemd-boot.enable = true;
    loader.grub.enable = false;
    loader.efi.canTouchEfiVariables = true;

    kernelPackages = pkgs.linuxPackages_latest;

    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" ];
      luks.devices =
        lib.listToAttrs (lib.concatLists (lib.attrValues cryptDevices));
    };
  };

  environment.systemPackages = [
    (pkgs.gpodder.overrideAttrs (drv: {
      propagatedBuildInputs = (drv.propagatedBuildInputs or []) ++ [
        pkgs.python3Packages.youtube-dl
      ];
    }))
    pkgs.paperwork
  ];

  # This is very ugly and I really want to avoid non-free packages on all
  # of my workstations. But right now I need to get rid of useless paper.
  nixpkgs.config.allowUnfreePredicate = pkg: let
    inherit (builtins.parseDrvName (pkg.name or "")) name;
  in name == "hplip";
  nixpkgs.overlays = lib.singleton (lib.const (super: {
    hplip = super.hplip.override { withPlugin = true; };
  }));

  hardware.cpu.amd.updateMicrocode = true;

  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.hplip ];

  hardware.enableRedistributableFirmware = true;

  networking.hostName = "dnyarri";
  networking.interfaces.enp1s0.useDHCP = true;

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/8C65-450D";
      fsType = "vfat";
    };
    "/" = {
      label = "dnyarri-root";
      fsType = "btrfs";
      options = [ "autodefrag" "space_cache" "compress=zstd" "noatime" ];
    };
  };

  services.btrfs.autoScrub.enable = true;

  swapDevices = map ({ name, ... }: {
    device = "/dev/mapper/${name}";
  }) cryptDevices.swap;

  users.users.aszlig.extraGroups = [
    "scanner"
    # TODO: Try to avoid this, but as there is only a single user using audio
    # on this machine, it's okay for now. But remember that this will break
    # heavily, should there be another user accessing the audio devices.
    "audio"
  ];

  services.xserver.videoDrivers = [ "amdgpu" ];
  services.xserver.xrandrHeads = [ "DisplayPort-0" "DisplayPort-1" ];
  services.xserver.wacom.enable = true;

  vuizvui.user.aszlig.services.i3.workspaces."1" = {
    label = "XMPP";
    assign = lib.singleton { class = "^(?:Tkabber|Gajim)\$"; };
  };

  vuizvui.user.aszlig.services.i3.workspaces."3" = {
    label = "Browser";
    assign = lib.singleton { class = "^Firefox\$"; };
  };
}
