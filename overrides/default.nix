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

    nixops = o: let
      master = everything.fetchgit {
        url = "git://github.com/NixOS/nixops.git";
        rev = "260af26504027b7ad11a6e38e02e0e4d7a820505";
        sha256 = "0nai6nkccl7qxq1756qip0zy14d68inj60ai7gnn1gd97pxn7yq0";
      };
      release = import "${master}/release.nix" {
        officialRelease = true;
      };
      build = getAttr o.stdenv.system release.build;
    in with everything; build.drvAttrs // {
      name = "nixops-1.3git";
      patches = (build.drvAttrs.patches or []) ++ [
        (fetchpatch {
          url = "https://github.com/NixOS/nixops/pull/201.diff";
          sha256 = "1i5yycqayxggg3l1i6wk8lp64lqlxw5nmfya9fcrgmck8ls0rxid";
        })
        (fetchpatch rec {
          name = "read-write-by-default.diff";
          url = "https://github.com/aszlig/nixops/compare/"
              + "NixOS:master...aszlig:${name}";
          sha256 = "0a1jcqrqfi7dfvlha5r0609bzvin7p7nj523xxcrvwpgp6ag0zsa";
        })
      ];
      patchFlags = "--merge -p1";
    };

    zsh = o: {
      postInstall = (o.postInstall or "") + ''
        find "$out" -type f -name zsh-newuser-install -delete
      '';
    };
  };

  # misc
  misc = {
    kernelSourceVuizvui = {
      version = "3.18.0-rc2";
      src = everything.fetchgit {
        url = git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git;
        rev = "9f76628da20f96a179ca62b504886f99ecc29223";
        sha256 = "0r26friccy3dywrdm963cxkxjkgqwgr5r7j61zz8wc0md3n8ici3";
      };
    };
  };
in allPackages // drvOverrides // argOverrides
