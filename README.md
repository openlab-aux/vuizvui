About Vuizvui
=============

This contains a [set of NixOS modules/configs][manual] and various other Nix
expressions used by OpenLab and its members.

The name "vuizvui" is of Bavarian origins and means something like "too much"
while on the other side "nix" means nothing. Which fits quite well because this
repository is for everything either too complex or not polished/generic enough
to be pushed into [nixpkgs].

Hydra builds: https://headcounter.org/hydra/jobset/openlab/vuizvui

[nixpkgs]:    https://github.com/NixOS/nixpkgs
[manual]:     https://headcounter.org/hydra/job/openlab/vuizvui/manual/latest/download/1/manual.html

## Installing a machine

To enable a vuizvui configuration, first set the nix channel to the corresponding channel build by the hydra, e.g.

    nix-channel --add https://headcounter.org/hydra/channel/custom/openlab/vuizvui/channels.machines.labnet.labtop vuizvui
    nix-channel --update

You have to use the same channel name as is defined by the option `vuizvui.channelName`, `vuizvui` by default.

Then import the `use` module in your configuration, like this:

```nix
{ pkgs, lib, ...}:
{
  imports = [ (import <vuizvui/machines> {}).labnet.labtop.use ];
}
```

    nixos-rebuild switch

All set.