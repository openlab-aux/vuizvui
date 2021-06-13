{ pkgs, ... }:

{
  config = {
    nix.trustedUsers = [ "lukas" ];
    users.users.lukas = {
      isNormalUser = true;
      uid = 1000;
      home = "/home/lukas";
      group = "users";
      extraGroups = [ "wheel" "networkmanager" "audio" "docker" "cdrom" ];
      shell = "${pkgs.fish}/bin/fish";
    };
  };
}
