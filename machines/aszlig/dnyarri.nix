{ config, pkgs, utils, lib, ... }:

let
  mkDevice = category: num: uuid: {
    name = "dnyarri-${category}-crypt-${toString num}";
    value.device = "/dev/disk/by-uuid/${uuid}";
  };

  cryptDevices = {
    root = lib.imap (mkDevice "root") [
      "b13d257e-b5fd-4f86-82b1-8bfe06335a75"
      "a607c827-2fd7-49d9-a7d8-05279c8653a4"
      "de32cb42-2e09-4e6a-84b4-244078d289c8"
      "12dac5b2-7647-45de-b752-5efee23855d0"
    ];
    swap = lib.imap (mkDevice "swap") [
      "e0a8281d-2c68-48ca-8e00-f0defaf51f38"
      "d26e61d6-c238-4c01-8c57-b1ba0bdb8c93"
    ];
  };

  bcacheMode = "writearound";

  bcacheStart = ''
    for i in /sys/block/bcache[0-9]*/bcache/cache_mode; do
      echo ${lib.escapeShellArg bcacheMode} > "$i"
    done
  '';

  bcacheStop = ''
    for i in /sys/block/bcache[0-9]*/bcache/cache_mode; do
      echo none > "$i"
    done
  '';

in {
  vuizvui.user.aszlig.profiles.workstation.enable = true;

  vuizvui.requiresTests = [
    ["vuizvui" "aszlig" "dnyarri" "luks2-bcache"]
  ];

  nix.maxJobs = 8;

  boot = {
    loader.systemd-boot.enable = true;
    loader.grub.enable = lib.mkForce false;
    loader.efi.canTouchEfiVariables = true;

    kernelPackages = pkgs.linuxPackages_latest;

    initrd = {
      availableKernelModules = [ "bcache" ];
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

  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.hplip ];

  vuizvui.system.kernel.bfq.enable = true;
  hardware.enableRedistributableFirmware = true;

  networking.hostName = "dnyarri";
  networking.interfaces.eno0.useDHCP = true;

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/9A75-9A6E";
      fsType = "vfat";
    };
    "/" = {
      label = "dnyarri-root";
      fsType = "btrfs";
      options = [ "autodefrag" "space_cache" "compress=zstd" "noatime" ];
    };
  };

  powerManagement.powerUpCommands = ''
    ${pkgs.hdparm}/sbin/hdparm -B 255 /dev/disk/by-id/ata-ST31500541AS_5XW0AMNH
    ${pkgs.hdparm}/sbin/hdparm -B 255 /dev/disk/by-id/ata-ST31500541AS_6XW0M217
    ${bcacheStart}
  '';

  powerManagement.powerDownCommands = bcacheStop;

  services.btrfs.autoScrub.enable = true;

  # Inject preStart/postStart for activating/deactivating bcache to the scrub
  # services, so we don't get large amounts of nonsense on the caching device.
  systemd.services = let
    scrubServiceUnits = let
      mkName = fs: "btrfs-scrub-${utils.escapeSystemdPath fs}.service";
    in map mkName config.services.btrfs.autoScrub.fileSystems;
  in lib.genAttrs scrubServiceUnits (lib.const {
    preStart = bcacheStop;
    postStart = bcacheStart;
  });

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

  services.xserver.videoDrivers = [ "ati" ];
  services.xserver.xrandrHeads = [ "DVI-0" "HDMI-0" ];

  vuizvui.user.aszlig.services.i3.workspaces."1" = {
    label = "XMPP";
    assign = lib.singleton { class = "^(?:Tkabber|Gajim)\$"; };
  };

  vuizvui.user.aszlig.services.i3.workspaces."3" = {
    label = "Browser";
    assign = lib.singleton { class = "^Firefox\$"; };
  };
}
