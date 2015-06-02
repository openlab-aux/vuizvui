# nixosconf
_NixOS configuration for my current machines._

Feel free to use and adapt as you wish.

In case you've wondered where to start, normally it's enough to simply import
the `entry-*.nix` file, respective of the machine you want to build, into your
`/etc/nixos/configuration.nix`, for example:
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
that whatever caused the error has not been yet included in the latest Hydra
channel update (this is assuming that you are using the _unstable_ channel).
Therefore your best bet is to either disable the culprit in the configuration
or just wait until the [unstable channel](https://hydra.nixos.org/job/nixos/trunk-combined/tested#tabs-constituents)
has been updated.
