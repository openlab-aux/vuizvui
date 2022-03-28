{ lib, pkgs, ... }:

let
  # Slightly modified version of
  # https://leahneukirchen.org/blog/archive/2020/05/a-minimal-vimrc.html
  vimrc = pkgs.writeText "vimrc" ''
    set nocp bs=2 hid is ru sm t_te= t_ti= vb wim=longest,list
    set ignorecase smartcase
  '';

  vim = pkgs.writeShellScriptBin "vim" ''
    exec "${lib.getBin pkgs.vim}/bin/vim" -u "${vimrc}" "$@"
  '';

in

{
  config = {
    environment = {
      systemPackages = [ vim ];
      variables = {
        # on non-servers this will be set to emacs in base-laptop.nix
        EDITOR = lib.mkDefault "${vim}/bin/vim";
        VISUAL = lib.mkDefault "${vim}/bin/vim";
      };
    };
  };
}
