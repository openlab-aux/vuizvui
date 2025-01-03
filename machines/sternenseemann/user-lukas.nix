{ pkgs, ... }:

{
  config = {
    nix.settings.trusted-users = [ "lukas" ];
    users.users.lukas = {
      isNormalUser = true;
      uid = 1000;
      home = "/home/lukas";
      group = "users";
      extraGroups = [ "wheel" "networkmanager" "audio" "cdrom" ];
      shell = "${pkgs.fish}/bin/fish";
    };
  };
}
