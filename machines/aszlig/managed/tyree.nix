{ config, pkgs, unfreeAndNonDistributablePkgs, lib, ... }:

{
  boot.initrd.availableKernelModules = [ "usbhid" ];
  boot.kernelModules = [ "kvm-intel" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  environment.systemPackages = with pkgs; [
    calibre cdparanoia chromium figlet gajim gimp htop inkscape thunderbird
    kdeApplications.gwenview kdeApplications.okular krita libreoffice mosh mpv
    pciutils vlc vuizvui.tomahawk wget wine youtubeDL
    unfreeAndNonDistributablePkgs.skype
  ];

  fileSystems."/boot".device = "/dev/disk/by-uuid/A0D5-269D";
  fileSystems."/boot".fsType = "vfat";

  fileSystems."/".label = "tyree-root";
  fileSystems."/".fsType = "btrfs";
  fileSystems."/".options = [
    "compress=lzo"
    "discard"
    "noatime"
    "space_cache"
    "ssd"
  ];

  swapDevices = lib.singleton {
    label = "tyree-swap";
  };

  hardware.cpu.intel.updateMicrocode = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  i18n.consoleUseXkbConfig = true;
  i18n.defaultLocale = "de_DE.UTF-8";

  networking.hostName = "tyree";
  networking.firewall.enable = false;
  networking.wireless.enable = false;
  networking.networkmanager.enable = true;
  networking.useNetworkd = true;

  # Temporary, seems to be a bug in NetworkManager:
  networking.usePredictableInterfaceNames = false;

  nix.maxJobs = 4;
  nix.useSandbox = true;
  nix.readOnlyStore = true;
  nix.buildCores = 0;
  nix.extraOptions = ''
    auto-optimise-store = true
  '';

  nixpkgs.config = {
    pulseaudio = true;
    chromium.enablePepperFlash = true;
  };

  programs.bash.enableCompletion = true;
  programs.bash.interactiveShellInit = lib.mkBefore ''
    export LANG=en_US.UTF-8
  '';

  services.openssh.enable = true;
  services.tlp.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "de";
  services.xserver.xkbOptions = "eurosign:e,caps:none";
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.wacom.enable = true;

  time.timeZone = "Europe/Berlin";

  vuizvui.user.aszlig.profiles.managed.enable = true;
  vuizvui.user.aszlig.profiles.managed.mainUser = "bla";

  vuizvui.hardware.t100ha.enable = true;
  vuizvui.user.aszlig.programs.vim.enable = true;
}
