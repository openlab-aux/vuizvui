{ makeSetupHook, libarchive, innoextract }:

{
  gogUnpackHook = makeSetupHook {
    deps = [ libarchive innoextract ];
  } ./gog-unpack.sh;
}
