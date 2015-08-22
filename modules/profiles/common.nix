{ config, lib, ... }:

with lib;

{
  options.vuizvui = {
    modifyNixPath = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to modify NIX_PATH for vuizvui, so that &lt;nixpkgs&gt; points
        to the path within the Nix channel instead of the
        <literal>nixpkgs</literal> or <literal>nixos</literal> channel from the
        root user.
      '';
    };

    channelName = mkOption {
      type = types.str;
      default = "vuizvui";
      description = ''
        The channel name which is used to refer to <literal>vuizvui</literal>.
      '';
    };
  };

  config = {
    nixpkgs.config.packageOverrides = pkgs: {
      inherit (import ../../pkgs {
        # We need to make sure to incorporate other package overrides,
        # otherwise we are unable to override packages in vuizvui.*.
        pkgs = pkgs // config.nixpkgs.config.packageOverrides pkgs;
      }) vuizvui;
    };

    nix.binaryCachePublicKeys = [
      "headcounter.org:/7YANMvnQnyvcVB6rgFTdb8p5LG1OTXaO+21CaOSBzg="
    ];

    nix.nixPath = let
      rootChannelsPath = "/nix/var/nix/profiles/per-user/root/channels";
      channelPath = "${rootChannelsPath}/${config.vuizvui.channelName}";
    in mkIf config.vuizvui.modifyNixPath (mkOverride 90 [
      "vuizvui=${channelPath}"
      "nixpkgs=${channelPath}/nixpkgs"
      "nixos-config=/etc/nixos/configuration.nix"
      rootChannelsPath
    ]);
  };
}
