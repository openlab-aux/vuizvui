{ config, pkgs, lib, ... }:

{
  vuizvui.user.devhell.profiles.base.enable = true;

  boot = {
    loader = {
      grub  = {
        enable = true;
        version = 2;
        copyKernels = true;
        devices = [ "/dev/sda" "/dev/sdb" ];
      };
    };

    zfs = {
      enableUnstable = true;
      requestEncryptionCredentials = true;
    };

    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
      kernelModules = [ "fuse" "amdgpu" ];
      postDeviceCommands = ''
        echo none > /sys/block/sda/queue/scheduler
        echo none > /sys/block/sdb/queue/scheduler
      '';
    };

    kernelParams = [ "pcie_aspm=off" ];
    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [ ];
    blacklistedKernelModules = [ ];
    kernelPackages = lib.mkForce
      config.boot.zfs.package.latestCompatibleLinuxPackages;
  };

  hardware = {
    cpu.amd.updateMicrocode = true;
    opengl = {
      enable = true;
      driSupport = true;
      extraPackages = with pkgs; [
        libvdpau-va-gl
        vaapiVdpau
        amdvlk
        rocm-opencl-icd
        rocm-opencl-runtime
      ];
    };
  };

  fileSystems."/" = {
    device = "zpool/root/nixos";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "zpool/home";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "ext4";
  };

  zramSwap.enable = true;

  # FIXME Check if this is still necessary in the future
  systemd.services.systemd-networkd-wait-online.enable = false;

  networking = {
    hostName = "gunnr";
    hostId = "29e6affc";
    wireless.enable = false;
    useNetworkd = true;
    interfaces.enp4s0.useDHCP = true;
  };

  nix = {
    settings.max-jobs = lib.mkDefault 16;
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

  #### Machine-specific service configuration ####

  vuizvui.user.devhell.profiles.services.enable = true;

  services = {
    printing = {
      enable = true;
      drivers = [ pkgs.foo2zjs pkgs.hplip ];
    };
    syncthing = {
      enable = true;
    };
  };

  services.zfs.autoScrub.enable = true;

  services.xserver = {
    enable = true;
    layout = "dvorak";
    videoDrivers = [ "amdgpu" ];

    # XXX: Factor out and make DRY, because a lot of the stuff here is
    # duplicated in the other machine configurations.
    displayManager.sessionCommands = ''
      ${pkgs.xbindkeys}/bin/xbindkeys &
      ${pkgs.nitrogen}/bin/nitrogen --restore &
      ${pkgs.rofi}/bin/rofi &
      ${pkgs.xorg.xrdb}/bin/xrdb "${pkgs.writeText "xrdb.conf" ''
        Xft.dpi:                     96
        Xft.antialias:               true
        Xft.hinting:                 full
        Xft.hintstyle:               hintslight
        Xft.rgba:                    rgb
        Xft.lcdfilter:               lcddefault
        Xft.autohint:                1
        Xcursor.theme:               Vanilla-DMZ-AA
        Xcursor.size:                22
        *.charClass:33:48,35:48,37:48,43:48,45-47:48,61:48,63:48,64:48,95:48,126:48,35:48,58:48
      ''}"
    '';
   };

   services.timesyncd = {
     servers = [ "ntp.canterbury.ac.uk" ];
   };

   #### Machine-specific packages configuration ####

   vuizvui.user.devhell.profiles.packages.enable = true;

   nixpkgs.config.mpv.vaapiSupport = true;
   nixpkgs.config.mpv.bs2bSupport = true;

   environment.systemPackages = with pkgs; [
     glxinfo
     libva
     libvdpau-va-gl
     vaapiVdpau
     vdpauinfo
     xbindkeys
   ];
}
