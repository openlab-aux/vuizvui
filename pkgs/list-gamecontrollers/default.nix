{ runCommandCC, pkg-config, SDL2 }:

runCommandCC "list-gamecontrollers" {
  buildInputs = [ pkg-config SDL2 ];
} ''
  mkdir -p "$out/bin"
  cc -Werror "${./list-gc.c}" \
    $(pkg-config --libs --cflags sdl2) \
    -o "$out/bin/list-gamecontrollers"
''
