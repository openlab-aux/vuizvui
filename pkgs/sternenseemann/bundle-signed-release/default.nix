# Build a directory containing release tarballs and
# their signatures. Fail if a signature is invalid.
{ lib
, getBins
, signify
, buildGitTarball
, runCommandNoCC
}:

{ # public key to verify against
  publicKey
  # directory signature files are located in
, sigs
}:

{ # project name:
  # * tarballs are name ${pname}-${tag}.tar.gz
  # * signatures are name ${pname}-${tag}.tar.gz.sig
  pname
  # information about the git remote to fetch from
  # must contain an url attribute and may contain
  # a subDir attribute.
, git
  # List of releases which are represented as an
  # attribute set which contains a sha256 and
  # either a tag or rev attribute.
, releases
}:

let
  bins = getBins signify [ "signify" ];

  tarballs = builtins.map
    (args: buildGitTarball (git // args // {
      inherit pname;
    })) releases;

  sigFor = tarball: "${sigs}/${tarball.name}.sig";
in

runCommandNoCC "${pname}-releases" {} (''
  mkdir -p "$out"
'' + lib.concatMapStrings (tarball: ''
  # verify tarball and inform user about what's happening
  echo -n "${tarball.name}: "
  ${bins.signify} -V \
    -p "${publicKey}" \
    -m "${tarball}" \
    -x "${sigFor tarball}"

  # succeeded, so copy tarball and signature
  ln -s "${tarball}" "$out/${tarball.name}"
  ln -s "${sigFor tarball}" "$out/${baseNameOf (sigFor tarball)}"
'') tarballs)
