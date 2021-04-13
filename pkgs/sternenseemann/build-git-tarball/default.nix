# Build a reproducible tar.gz from a git revision or tag
{ lib
, fetchgit
, runCommandNoCC
, gnutar
, gzip
, getBins
}:

{ url
, sha256
, pname
, subDir ? ""
, ...
}@args:

assert lib.assertMsg (args ? rev || args ? tag) "Need either rev or tag";

let
  bins = getBins gzip [ "gzip" ]
      // getBins gnutar [ "tar" ]
      ;

  shortRev = args.tag or args.rev;
  realRev =
    if args ? tag
    then "refs/tags/${args.tag}"
    else args.rev;

  source = fetchgit {
    inherit url sha256;
    rev = realRev;
  };

  basename = "${pname}-${shortRev}";
in

runCommandNoCC "${basename}.tar.gz" {} ''
  cd ${source}${subDir}
  ${bins.tar} -c ./ --transform 's/^\./${basename}/' | ${bins.gzip} > $out
''
