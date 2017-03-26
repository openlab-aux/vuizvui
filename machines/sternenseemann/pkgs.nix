{ pkgs, lib }:

let

  mpv = pkgs.mpv.override { scripts = [ pkgs.mpvScripts.convert ]; };

  texlive = with pkgs.texlive; combine { inherit scheme-medium minted units collection-bibtexextra wrapfig libertine; };

  urxvt = pkgs.rxvt_unicode-with-plugins.override { plugins = [ pkgs.urxvt_perls ]; };

in { inherit mpv texlive urxvt; }
