{ config, pkgs, lib, ... }:

let
  inherit (lib) mkOption types;
  rootChannelsPath = "/nix/var/nix/profiles/per-user/root/channels";
  channelPath = "${rootChannelsPath}/${config.vuizvui.channelName}";

in {
  options.vuizvui = {
    modifyNixPath = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to modify {env}`NIX_PATH` for vuizvui, so that `<nixpkgs>` points
        to the path within the Nix channel instead of the
        `nixpkgs` or `nixos` channel from the root user.
      '';
    };

    enableGlobalNixpkgsConfig = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Enabling this links `nixos-config` to be used by
        `nixpkgs-config`, which essentially means that
        attributes defined in {option}`nixpkgs.config` are also in
        effect for user environments.
      '';
    };

    channelName = mkOption {
      type = types.str;
      default = "vuizvui";
      description = ''
        The channel name which is used to refer to `vuizvui`.
      '';
    };
  };

  config = {
    # Expose all packages in ../../pkgs as pkgs.vuizvui in modules.
    nixpkgs.overlays = lib.mkBefore (lib.singleton (lib.const (super: {
      vuizvui = import ../../pkgs { pkgs = super; };
    })));

    nix = {
      settings = {
        substituters = [ "https://hydra.build/" ];
        trusted-public-keys = [
          "headcounter.org:/7YANMvnQnyvcVB6rgFTdb8p5LG1OTXaO+21CaOSBzg="
        ];
      };
    };

    environment.variables.NIXPKGS_CONFIG = let
      inherit (config.vuizvui) enableGlobalNixpkgsConfig;
      nixpkgsCfg = toString (pkgs.writeText "nixpkgs-try-config.nix" ''
        if (builtins.tryEval <nixpkgs-config>).success
        then import <nixpkgs-config>
        else {}
      '');
    in lib.mkIf enableGlobalNixpkgsConfig (lib.mkForce nixpkgsCfg);

    nix.nixPath = let
      nixosConfig = "/etc/nixos/configuration.nix";
      nixpkgsConfig = "nixpkgs-config=${pkgs.writeText "nixpkgs-config.nix" ''
        (import ${pkgs.path}/nixos/lib/eval-config.nix {
          modules = [ ${nixosConfig} ];
        }).config.nixpkgs.config
      ''}";
      nixPath = [
        "vuizvui=${channelPath}"
        "nixpkgs=${channelPath}/nixpkgs"
        "nixos-config=${nixosConfig}"
        rootChannelsPath
      ] ++ lib.optional config.vuizvui.enableGlobalNixpkgsConfig nixpkgsConfig;
    in lib.mkIf config.vuizvui.modifyNixPath (lib.mkOverride 90 nixPath);

    programs.command-not-found = {
      # NixOS 25.11 disables command-not-found by default because it doesn't work
      # with flake based systems. This doesn't affect vuizvui.
      enable = lib.mkDefault true;
      # correct path used by command-not-found
      dbPath = lib.mkDefault "${channelPath}/nixpkgs/programs.sqlite";
    };
  };
}
