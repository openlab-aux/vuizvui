{ pkgs, lib, ... }:

{
  vuizvui.user.aszlig.profiles.base.enable = true;

  boot.initrd.availableKernelModules = [ "sdhci_acpi" ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.loader.grub.enable = lib.mkForce false;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;

  networking.hostName = "meshuggah";

  nix.maxJobs = 2;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/9bddc8d3-88ee-4aac-b885-c9abca36b863";
    fsType = "btrfs";
    options = [
      "compress=lzo"
      "discard"
      "noatime"
      "space_cache"
      "ssd"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/A318-8495";
    fsType = "vfat";
  };

  services.openssh.enable = true;
  services.openssh.permitRootLogin = "without-password";

  swapDevices = lib.singleton {
    device = "/dev/disk/by-uuid/2738fbe5-fee8-4835-a512-241f447252fa";
  };

  users.users.root.openssh.authorizedKeys.keys = let
    mkKey = name: type: data: "${type} ${lib.concatStrings data} ${name}";
  in lib.singleton (mkKey "openpgp:0x6321DF96" "ssh-rsa" [
    "AAAAB3NzaC1yc2EAAAADAQABAAACAQCWc5omkAV4yV9gn11kHPlxfSXHlIROkJZAmn"
    "towEMeUAyOI38gc3QNCTYRpo8bpD68U4X/p0NHIetm9v4t9t8Nsz/Tj3KHmh291DIj"
    "W4IisrjCX1o9aj5ESu2bCNp+6oEWjb2GbecJjn3kf4eh82imh4F9s0PtGzCUB+iYgP"
    "OiMCj4pfMK76yu6SKoyU43FwhkD+v5DgmBT0+GPftce4Dyrh3GV8qUotP32hohsFq3"
    "aWxcRU6Y3yRwRiUskh1B6+H3W44peX+F+0j6jnX49DwAwXfFVAmqDqxLz4r37uRjYe"
    "G+6UGt6fm2hgYyLf07ph8c6fQCOmYaPs7lvpBDB+zuadxAlk/bKHKgfr4xbXyoIjAV"
    "L7uw3ui0NqbAMoBGEgi+Sk6t7JYQsyhvshauw3TyTi3Jjr1NBjHmbCNgguNLtWoFsJ"
    "dd8Jgd1AVQbxnsLitWxSrTem9mlBRYWV+SxyBR5kqhrw36/yXVeWp+jX23Kg98Bco7"
    "NA+QPWKXKE4HffAZQ3MJGEZJkS+9l1wCjrhAa5jjbH4Auh+bRNkKImXg26OdU7fU1O"
    "Lgzi3J1vmcLV/HOqp66i8/biehsYL9GL2uHaqZS9Xaj177bTqveaVRjyomiNCkm0ed"
    "+NPSboEuGyEE59D8w7FBsk/d3r3z3fZg1j++UvSwVkqXYByl5pzCnw=="
  ]);
}
