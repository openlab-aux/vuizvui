{ config, pkgs, lib, ... }:

let
  inherit (config.boot.kernelPackages) kernel;

  modBaseDir = "kernel/drivers/net/wireless";

  rtl8192cu = pkgs.stdenv.mkDerivation {
    name = "rtl8192cu-${kernel.version}";

    src = pkgs.fetchFromGitHub {
      owner = "pvaret";
      repo = "rtl8192cu-fixes";
      rev = "f3edeeef68808f0f6fc06b5e36b559e1d06232ab";
      sha256 = "0v0rrxfmvi9flrg3xns826a6n1mlgd3vs5z2x59aqvwfj5b4rv7b";
    };

    postPatch = ''
      substituteInPlace Makefile --replace /sbin/depmod :
    '';

    makeFlags = [
      "BUILD_KERNEL=${kernel.modDirVersion}"
      "KSRC=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
      "MODDESTDIR=$(out)/lib/modules/${kernel.modDirVersion}/${modBaseDir}/"
    ];

    preInstall = ''
      mkdir -p "$out/lib/modules/${kernel.modDirVersion}/${modBaseDir}"
    '';

    enableParallelBuilding = true;
  };

in {
  options.vuizvui.hardware.rtl8192cu = {
    enable = lib.mkEnableOption "support for RTL8192CU wireless chipset";
  };

  config = lib.mkIf config.vuizvui.hardware.rtl8192cu.enable {
    boot.extraModulePackages = [ rtl8192cu ];
    # Note that the module is called "8192cu" so we don't blacklist the module
    # we actually want to use. The ones we blacklist here are the modules from
    # the mainline kernel, which unfortunately do not seem to work very well.
    boot.blacklistedKernelModules = [ "rtl8192cu" "rtl8192c_common" "rtlwifi" ];
  };
}
