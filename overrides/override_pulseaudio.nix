{ config, pkgs, lib, ... }:

{
  hardware = {
    pulseaudio.package = pkgs.pulseaudio.override {
      useSystemd = true;
    };
  };
}
