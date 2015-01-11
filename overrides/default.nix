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

    i3 = o: {
      patches = (o.patches or []) ++ (singleton (everything.fetchurl {
        url = "http://bugs.i3wm.org/report/raw-attachment/ticket/1332/"
            + "i3-validate-config-without-x.patch";
        sha256 = "1njmrvqr3h5wf8dwg5di136cjvnn5miaj7by3q93x8028hdpigag";
      }));
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
      version = "3.19.0-rc3";
      src = everything.fetchgit {
        url = git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git;
        rev = "4850d37d3a7c049f7dc3eb09d7ae4e5553ac521b";
        sha256 = "019k4wcyirfc286kzywzj0548mwq5y6gxs4512gjagpd1bzxscds";
      };
    };
  };
in allPackages // drvOverrides // argOverrides
