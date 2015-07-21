# nixosconf
_NixOS configuration for my current machines._

Feel free to use and adapt as you wish.

In case you've wondered where to start, normally it's enough to simply import
the `entry-*.nix` file, respective of the machine you want to build (naturally
you can also just rename the files to reflect your own configuration/machine
names), into your `/etc/nixos/configuration.nix`, for example:
```
{ config, pkgs, lib, ... }:

{
  imports =
    [
      /home/<username>/nixosconf/entry-<machinename>.nix
    ];
}
```

**NOTE:** This NixOS configuration is geared towards people running NixOS
_unstable_ or `nixpkgs` [master branch](https://github.com/NixOS/nixpkgs). This
also means that if `nixos-rebuild switch` encounters an error, then it is likely
that whatever caused the error has not yet been included in the latest Hydra
channel update (this is assuming that you are using the _unstable_ channel).
Therefore your best bet is to either disable the culprit in the configuration
or just wait until the [unstable channel](https://hydra.nixos.org/job/nixos/trunk-combined/tested#tabs-constituents)
has been updated. Or, of course, run directly from `nixpkgs` master branch.

**NOTE 2:** Unless you have access to a Hydra build farm/machine you will most
likely want to remove the `binaryCaches` option in the `machine_common.nix`
file. This also means that you can ignore the `release.nix` file. Unless, of
course, you have access to a Hydra, in which case you'll still want to adapt
the `binaryCache` option.
