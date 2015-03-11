pkgs:

with pkgs.lib;

let
  allPackages = (import ../pkgs { pkgs = everything; }).vuizvui // misc;
  everything = pkgs // allPackages // drvOverrides // argOverrides;

  mapOverride = overrideFun: includePackages: let
    packages = pkgs // allPackages // includePackages;
    overrideName = name: overrideFun (getAttr name packages);
  in mapAttrs overrideName;

  # input attrset overrides using pkg.override
  argOverrides = mapOverride (getAttr "override") drvOverrides {
    netrw.checksumType = "mhash";
    pulseaudio.useSystemd = true;
    w3m.graphicsSupport = true;
    uqm.use3DOVideos = true;
    uqm.useRemixPacks = true;
    miro.enableBonjour = true;
  };

  gajimGtkTheme = everything.writeText "gajim.gtkrc" ''
    style "default" {
      fg[NORMAL] = "#d5faff"
      fg[ACTIVE] = "#fffeff"
      fg[SELECTED] = "#fffeff"
      fg[INSENSITIVE] = "#85aaaf"
      fg[PRELIGHT] = "#d7f2ff"

      text[NORMAL] = "#fffefe"
      text[ACTIVE] = "#fffeff"
      text[SELECTED] = "#fffeff"
      text[INSENSITIVE] = "#85aaaf"
      text[PRELIGHT] = "#d7f2ff"

      bg[NORMAL] = "#0f4866"
      bg[ACTIVE] = "#0c232e"
      bg[SELECTED] = "#005a56"
      bg[INSENSITIVE] = "#103040"
      bg[PRELIGHT] = "#1d5875"

      base[NORMAL] = "#0c232e"
      base[ACTIVE] = "#0f4864"
      base[SELECTED] = "#005a56"
      base[INSENSITIVE] = "#103040"
      base[PRELIGHT] = "#1d5875"
    }

    class "GtkWidget" style "default"

    gtk-enable-animations = 0
  '';

  gajimPatch = everything.substituteAll {
    src = ../pkgs/gajim/config.patch;
    nix_config = everything.writeText "gajim.config"
      (import ../cfgfiles/gajim.nix);
  };

  # derivation overrides
  drvOverrides = mapOverride overrideDerivation argOverrides {
    gajim = o: {
      patches = (o.patches or []) ++ singleton gajimPatch;
      postPatch = (o.postPatch or "") + ''
        sed -i -e '/^export/i export GTK2_RC_FILES="${gajimGtkTheme}"' \
          scripts/gajim.in
      '';
    };

    mpv = o: {
      installPhase = o.installPhase + ''
        cat > "$out/etc/mpv/mpv.conf" <<CONFIG
        ao=pulse
        CONFIG
      '';
    };

    zsh = o: {
      postConfigure = (o.postConfigure or "") + ''
        sed -i -e '/^name=zsh\/newuser/d' config.modules
      '';
    };
  };

  # misc
  misc = {
    kernelSourceVuizvui = {
      version = "4.0.0-rc3";
      src = everything.fetchgit {
        url = git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git;
        rev = "affb8172de395a6e1db52ed9790ca0456d8c29a9";
        sha256 = "1zqnkds0mglldm1syv17gl8n4wl1vy7rrh2vf3iga5w2psqwkyj4";
      };
    };
  };
in allPackages // drvOverrides // argOverrides
