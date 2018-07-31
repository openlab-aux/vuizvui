{ makeSetupHook, libarchive, innoextract }:

{
  fixFmodHook = makeSetupHook {
    deps = [];
  } ./fix-fmod.sh;

  gogUnpackHook = makeSetupHook {
    deps = [ libarchive innoextract ];
  } ./gog-unpack.sh;
}
