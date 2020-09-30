{ config, pkgs, lib, ... }:

{
  vuizvui.user.devhell.profiles.base.enable = true;
  vuizvui.system.kernel.bfq.enable = true;

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
      kernelModules = [ "fuse" ];
    };

    kernelParams = [ "pcie_aspm=off" ];
    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [ ];
    blacklistedKernelModules = [ ];
  };

  hardware = {
    cpu.amd.updateMicrocode = true;
    opengl = {
      enable = true;
      extraPackages = [ pkgs.libvdpau-va-gl pkgs.vaapiVdpau ];
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
    maxJobs = lib.mkDefault 16;
    extraOptions = ''
      auto-optimise-store = true
    '';
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

  services.zfs.autoScrub.enable = true;

  services.xserver = {
    enable = true;
    layout = "dvorak";
    videoDrivers = [ "modesetting" ];

    # XXX: Factor out and make DRY, because a lot of the stuff here is
    # duplicated in the other machine configurations.
    displayManager.sessionCommands = ''
      ${pkgs.xbindkeys}/bin/xbindkeys &
      ${pkgs.nitrogen}/bin/nitrogen --restore &
      ${pkgs.xscreensaver}/bin/xscreensaver -no-splash &
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
        
        ! Copyright (c) 2016-present Arctic Ice Studio <development@arcticicestudio.com>
        ! Copyright (c) 2016-present Sven Greb <code@svengreb.de>
        
        ! Project:    Nord XResources
        ! Version:    0.1.0
        ! Repository: https://github.com/arcticicestudio/nord-xresources
        ! License:    MIT
        
        #define nord0 #2E3440
        #define nord1 #3B4252
        #define nord2 #434C5E
        #define nord3 #4C566A
        #define nord4 #D8DEE9
        #define nord5 #E5E9F0
        #define nord6 #ECEFF4
        #define nord7 #8FBCBB
        #define nord8 #88C0D0
        #define nord9 #81A1C1
        #define nord10 #5E81AC
        #define nord11 #BF616A
        #define nord12 #D08770
        #define nord13 #EBCB8B
        #define nord14 #A3BE8C
        #define nord15 #B48EAD
        
        *.foreground:   nord4
        *.background:   nord0
        *.cursorColor:  nord4
        *fading: 35
        *fadeColor: nord3
        
        *.color0: nord1
        *.color1: nord11
        *.color2: nord14
        *.color3: nord13
        *.color4: nord9
        *.color5: nord15
        *.color6: nord8
        *.color7: nord5
        *.color8: nord3
        *.color9: nord11
        *.color10: nord14
        *.color11: nord13
        *.color12: nord9
        *.color13: nord15
        *.color14: nord7
        *.color15: nord6
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
     teams
     vaapiVdpau
     vdpauinfo
     xbindkeys
   ];
}
