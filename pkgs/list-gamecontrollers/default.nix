{ runCommand, pkgconfig, SDL2 }:

runCommand "list-gamecontrollers" {
  buildInputs = [ pkgconfig SDL2 ];
} ''
  mkdir -p "$out/bin"
  gcc -Werror "${./list-gc.c}" \
    $(pkg-config --libs --cflags sdl2) \
    -o "$out/bin/list-gamecontrollers"
''
