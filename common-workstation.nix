{ pkgs, config, lib, ... }:

let
  randrHeads = config.services.xserver.xrandrHeads;
in {
  imports = [
    ./common.nix
    ./packages.nix
    <nixpkgs/nixos/modules/programs/virtualbox.nix>
  ];

  boot.kernelParams = [ "zswap.enabled=1" ];
  boot.cleanTmpDir = true;

  environment.systemPackages = with lib; let
    mkRandrConf = acc: name: acc ++ singleton {
      inherit name;
      value = "--output '${name}' --preferred"
            + optionalString (acc != []) " --right-of '${(head acc).name}'";
    };
    randrConf = map (getAttr "value") (foldl mkRandrConf [] randrHeads);
  in singleton (pkgs.writeScriptBin "xreset" ''
    #!${pkgs.stdenv.shell}
    ${pkgs.xorg.xrandr}/bin/xrandr ${concatStringsSep " " randrConf}
  '');

  hardware = {
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudio.override {
      useSystemd = true;
    };
    opengl = {
      driSupport32Bit = true;
      s3tcSupport = true;
    };
  };

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = [
      pkgs.dosemu_fonts
      pkgs.liberation_ttf
    ];
  };

  vuizvui.i3.enable = true;
  vuizvui.slim.enable = true;
  vuizvui.vlock.enable = true;
  vuizvui.zsh.enable = true;

  vuizvui.git.enable = true;
  vuizvui.git.config = {
    color.ui = "auto";
    merge.tool = "vimdiff3";
    user.email = "aszlig@redmoonstudios.org";
    user.name = "aszlig";
    user.signkey = "8C2DC961";
    gpg.program = "${pkgs.gnupg}/bin/gpg2";
    push.default = "current";
    tar."tar.xz".command = "${pkgs.xz}/bin/xz -c";
    rebase.autosquash = true;
    rerere.enabled = true;
    rerere.autoupdate = true;
    commit.gpgsign = true;

    alias.backport = let
      release = "14.04";
      message = "Merge release ${release} into backports.";
    in "!git fetch upstream release-${release} &&"
     + " git merge -m \"${message}\" --log FETCH_HEAD";
  };

  services = {
    openssh = {
      enable = true;
      permitRootLogin = "without-password";
    };

    xfs.enable = false;

    gpm = {
      enable = true;
      protocol = "exps2";
    };

    printing.enable = true;

    udev.extraRules = ''
      SUBSYSTEM=="tty", ATTRS{idVendor}=="10c4", ATTRS{idProduct}=="ea60", \
        ATTRS{serial}=="0001", OWNER="aszlig", SYMLINK+="axbo"
      SUBSYSTEM=="usb*|tty", ACTION=="add|change", ATTRS{idVendor}=="0403", \
        ATTRS{idProduct}=="6001", OWNER="aszlig" # Enttec
    '';

    xserver = {
      enable = true;
      layout = "dvorak";

      startGnuPGAgent = true;

      displayManager.sessionCommands = ''
        ${pkgs.redshift}/bin/redshift -l 48.428404:10.866007 &
        ${pkgs.xorg.xrdb}/bin/xrdb "${pkgs.writeText "xrdb.config" ''
          Rxvt*font:                 vga
          Rxvt*background:           black
          Rxvt*foreground:           grey
          Rxvt*scrollBar:            false
          Rxvt*saveLines:            2000

          Rxvt*keysym.Home:          \033[1~
          Rxvt*keysym.End:           \033[4~

          Rxvt*urgentOnBell:         true

          XTerm*font:                vga
          XTerm*saveLines:           10000
          XTerm*bellIsUrgent:        true
          XTerm*background:          black
          XTerm*foreground:          grey

          XTerm*backarrowKeyIsErase: true
          XTerm*ptyInitialErase:     true
        ''}"
      '';

      desktopManager.default = "none";
      desktopManager.xterm.enable = false;
    };
  };

  users.extraUsers.aszlig = {
    uid = 1000;
    description = "aszlig";
    group = "users";
    extraGroups = [ "vboxusers" "wheel" "video" ];
    home = "/home/aszlig";
    useDefaultShell = true;
    createHome = true;
  };
}
