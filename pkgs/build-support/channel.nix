{ stdenv }:

{ name, src, constituents ? [], ... }@args:

let
  channel = stdenv.mkDerivation ({
    inherit name src constituents;
    preferLocalBuild = true;
    _hydraAggregate = true;

    phases = [ "unpackPhase" "patchPhase" "installPhase" ];
    installPhase = ''
      mkdir -p "$out/tarballs" "$out/nix-support"

      tar cJf "$out/tarballs/nixexprs.tar.xz" \
        --owner=0 --group=0 --mtime="1970-01-01 00:00:00 UTC" \
        --transform='s!^\.!${name}!' .

      echo "channel - $out/tarballs/nixexprs.tar.xz" \
        > "$out/nix-support/hydra-build-products"

      echo $constituents > "$out/nix-support/hydra-aggregate-constituents"
      for i in $constituents; do
        if [ -e "$i/nix-support/failed" ]; then
          touch "$out/nix-support/failed"
        fi
      done
    '';
  } // removeAttrs args [ "name" "channelName" "src" "constituents" ]);
in channel // {
  # XXX: We're adding meta.isHydraChannel outside of mkDerivation to avoid a
  # typecheck on meta since it doesn't include isHydraChannel.
  meta = (channel.meta or {}) // {
    isHydraChannel = true;
  };
}
