{ stdenv, hostPlatform, fetchurl, perl, pkgs, buildLinux, ... }@args:

let
  nixpkgs = import ../nixpkgs-path.nix;
  generic = "${nixpkgs}/pkgs/os-specific/linux/kernel/generic.nix";
in import generic (args // rec {
  extraMeta.branch = "4.12";
  version = "4.12.14";

  src = fetchurl {
    url = "mirror://kernel/linux/kernel/v4.x/linux-${version}.tar.xz";
    sha256 = "09zxmknh6awhqmj8dyq95bdlwcasryy35hkjxjlzixdgn52kzaw6";
  };

  kernelPatches = (args.kernelPatches or []) ++ [
    pkgs.kernelPatches.bridge_stp_helper
    pkgs.kernelPatches.p9_fixes
    pkgs.kernelPatches.cpu-cgroup-v2."4.11"
    pkgs.kernelPatches.modinst_arg_list_too_long
  ];

  extraMeta.hydraPlatforms = [];
} // (args.argsOverride or {}))
