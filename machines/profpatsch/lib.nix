{ lib, pkgs }:
rec {
  fish = pkgs.fish;

  authKeys = ["ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJhthfk38lzDvoI7lPqRneI0yBpZEhLDGRBpcXzpPSu+V0YlgrDix5fHhBl+EKfw4aeQNvQNuAky3pDtX+BDK1b7idbz9ZMCExy2a1kBKDVJz/onLSQxiiZMuHlAljVj9iU4uoTOxX3vB85Ok9aZtMP1rByRIWR9e81/km4HdfZTCjFVRLWfvo0s29H7l0fnbG9bb2E6kydlvjnXJnZFXX+KUM16X11lK53ilPdPJdm87VtxeSKZ7GOiBz6q7FHzEd2Zc3CnzgupQiXGSblXrlN22IY3IWfm5S/8RTeQbMLVoH0TncgCeenXH7FU/sXD79ypqQV/WaVVDYMOirsnh/ philip@nyx"];

  philip = rec {
    name = "philip";
    extraGroups = [ "wheel" "networkmanager" "docker" "vboxuser" "libvirtd" ];
    uid = 1000;
    createHome = true;
    home = "/home/philip";
    passwordFile = "${home}/.config/passwd";
    shell = "${lib.getBin fish}/bin/fish";
    openssh.authorizedKeys.keys = authKeys;
    isNormalUser = true;
  };
}
