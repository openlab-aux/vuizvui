{ lib
, writeRustSimpleBin
, testRustSimple
, temp
}:

testRustSimple (writeRustSimpleBin "nman" {
  meta = {
    license = lib.licenses.gpl3Only;
    description = "Open man page in a temporary nix-shell";
  };
  dependencies = [
    temp
  ];
  postInstall = ''
    install -Dm644 ${./nman.1} "$out/share/man/man1/nman.1"
  '';
} ./nman.rs)
