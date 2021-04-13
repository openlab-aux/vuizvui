{ runCommandWith
, stdenv
, lib
, lowdown
, chroma
, substituteAll
}:

let
  src = substituteAll {
    src = ./main.c;
    chroma = "${lib.getBin chroma}/bin/chroma";
    lowdown = "${lib.getBin lowdown}/bin/lowdown";
  };
in

runCommandWith {
  name = "schmecgit";
  inherit stdenv;
  derivationArgs.meta = {
    description = "schmeck it, about and source filter for cgit";
    license = lib.licenses.gpl2Plus;
    platforms = lib.platforms.unix;
  };
} ''
  mkdir -p "$out/bin"

  # compile main executable
  clang -o "$out/bin/schmecgit" -pedantic -Wall -Wextra -Werror -std=c99 ${src}

  # wrapper script for cgit about filter
  cat > "$out/bin/schmecgit-about" <<EOF
  #!${stdenv.shell}
  exec "$out/bin/schmecgit" --about \$@
  EOF
  chmod +x "$out/bin/schmecgit-about"
''
