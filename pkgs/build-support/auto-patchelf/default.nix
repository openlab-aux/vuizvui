{ stdenv, substituteAll, patchelf, binutils-unwrapped, findutils, file, glibc
, gnugrep, gnused
}:

substituteAll {
  src = ./setup-hook.sh;

  inherit (stdenv) shell;
  file = "${file}/bin/file";
  find = "${findutils}/bin/find";
  ldd = "${glibc.bin}/bin/ldd";
  objdump = "${binutils-unwrapped}/bin/objdump";
  patchelf = "${patchelf}/bin/patchelf";
  sed = "${gnused}/bin/sed";
}
