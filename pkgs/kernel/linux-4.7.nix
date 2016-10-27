{ stdenv, fetchurl, perl, buildLinux, pkgs, ... } @ args:

let
  nixpkgs = import ../../nixpkgs-path.nix;
  generic = "${nixpkgs}/pkgs/os-specific/linux/kernel/generic.nix";
in import generic (args // rec {
  version = "4.7.10";
  extraMeta.branch = "4.7";

  src = fetchurl {
    url = "mirror://kernel/linux/kernel/v4.x/linux-${version}.tar.xz";
    sha256 = "1p2r5d0jcrak9gxp0339g9z198x9laf09h08ck4jllhhaajrnicj";
  };

  kernelPatches = (args.kernelPatches or []) ++ [
    pkgs.kernelPatches.cpu-cgroup-v2."4.7"
  ];

  features.iwlwifi = true;
  features.efiBootStub = true;
  features.needsCifsUtils = true;
  features.canDisableNetfilterConntrackHelpers = true;
  features.netfilterRPFilter = true;
} // (args.argsOverride or {}))
