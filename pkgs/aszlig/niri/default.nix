{ lib, replaceVarsWith, applyPatches, fetchFromGitHub, rustPlatform, niri
, brightnessctl, systemd, mako, catppuccin-cursors, xwayland-satellite

, outputs ? {}
}:

niri.overrideAttrs (drv: let
  patches = (drv.patches or []) ++ [
    (replaceVarsWith {
      name = "config.patch";
      src = ./config.patch.in;

      replacements = {
        brightnessctl = "${lib.getBin brightnessctl}/bin/brightnessctl";
        systemctl = "${lib.getBin systemd}/bin/systemctl";
        makoctl = "${lib.getBin mako}/bin/makoctl";
        xcursorPath = "${catppuccin-cursors.frappeLavender}/share/icons";

        xwaylandSatellite = "${
          lib.getBin xwayland-satellite
        }/bin/xwayland-satellite";

        # These values are special identifiers for wpctl which we need to keep
        # unaltered.
        DEFAULT_AUDIO_SINK = null;
        DEFAULT_AUDIO_SOURCE = null;
      };
    })
    # HDR support patches rebased against Niri 26.04, discussed at:
    # https://github.com/niri-wm/niri/discussions/1128#discussioncomment-17472733
    hdr-patches/0001-feat-add-per-output-max-bpc-config-and-ipc-action.patch
    hdr-patches/0002-feat-add-10-bit-framebuffer-pixel-format.patch
    hdr-patches/0003-wiki-Clarify-max-bpc-docs.patch
    hdr-patches/0004-Remove-deprecated-keep-max-bpc-unchanged-debug-flag.patch
    hdr-patches/0005-feat-initial-HDR-support-for-fullscreen-applications.patch
    hdr-patches/0006-hdr-apply-connector-color-state-via-smithay-s-atomic.patch
    hdr-patches/0007-hdr-advertise-HDR-to-clients-via-color-management-fe.patch
    hdr-patches/0008-hdr-search-the-surface-tree-for-HDR-image-descriptio.patch
    hdr-patches/0009-hdr-composite-SDR-content-into-the-blend-space-of-al.patch
    hdr-patches/0010-hdr-resolve-subsurfaces-to-their-root-for-preferred-.patch
  ];

  outputConfig = lib.concatMapAttrsStringSep "\n\n" (output: lines: let
    # This is to indent the content within the output section by two
    # spaces.
    splitted = lib.splitString "\n" lines;
    firstLine = lib.head splitted + "\n";
    mkIndent = line: "  ${line}";
    rest = lib.concatMapStringsSep "\n" mkIndent (lib.tail splitted);
  in ''
    output "${lib.escape ["\"" "\\"] output}" {
      ${if splitted == [] then "" else firstLine + rest}
    }
  '') outputs;

in {
  inherit patches;

  postPatch = (drv.postPatch or "") + ''
    ln -s ${applyPatches {
      patches = [
        # This is to make sure we send luminances and target_luminances
        # accordingly, see:
        # https://wayland.app/protocols/color-management-v1#wp_image_description_info_v1
        ./smithay-luminance.patch
      ];
      src = fetchFromGitHub {
        owner = "dividebysandwich";
        repo = "smithay";
        rev = "b5c067f04bd6b9298489022d23ad67f8f10aaf53";
        hash = "sha256-zIsscQPeN2zSJFM84sy56Li3iyqBpLJhGCPopFkLeJ4";
      };
    }} ../smithay

    substituteInPlace resources/default-config.kdl \
      --replace-fail '/*#OUTPUT_CONFIG#*/' ${lib.escapeShellArg outputConfig}
  '';

  cargoDeps = rustPlatform.fetchCargoVendor {
    inherit (drv) src;
    inherit patches;
    hash = "sha256-KPG9UacVtxVbyCNtmAmPjFNbNtaKHRRVPAMbMeq9GoM";
  };

  postInstallCheck = (drv.postInstallCheck or "") + ''
    "$out/bin/niri" validate --config resources/default-config.kdl
  '';
})
