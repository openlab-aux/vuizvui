{ makeSetupHook, libarchive, innoextract }:

{
  fixFmodHook = makeSetupHook {
    name = "fix-fmod-hook";
    deps = [];
  } ./fix-fmod.sh;

  gogUnpackHook = makeSetupHook {
    name = "gog-unpack-hook";
    propagatedBuildInputs = [ libarchive innoextract ];
  } ./gog-unpack.sh;
}
