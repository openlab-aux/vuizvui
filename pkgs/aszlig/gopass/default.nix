{ lib, buildGoModule, fetchFromGitHub, installShellFiles, makeWrapper
, git, gnupg, xclip, wl-clipboard

, gopass
}:

assert lib.versionOlder gopass.version "1.10.2";

buildGoModule rec {
  pname = "gopass";
  version = "1.9.2";

  src = fetchFromGitHub {
    owner = "gopasspw";
    repo = pname;
    rev = "v${version}";
    sha256 = "066dphw8xq0g72kj64sdai2yyllnr6ca27bfy5sxhk8x69j97rvz";
  };

  patches = [ ./use-color-in-pager.patch ];
  vendorSha256 = "1wn20bh7ma4pblsf6qnlbz5bx4p9apig3d1yz7cpsqv4z3w07baw";
  nativeBuildInputs = [ installShellFiles makeWrapper ];
  doCheck = false;

  buildFlagsArray = [
    "-ldflags=-s -w -X main.version=${version} -X main.commit=${src.rev}"
  ];

  postInstall = ''
    for shell in bash fish zsh; do
      $out/bin/gopass completion "$shell" > "gopass.$shell"
      installShellCompletion "gopass.$shell"
    done
  '';

  postFixup = let
    wrapperPath = lib.makeBinPath [ git gnupg xclip wl-clipboard ];
    extraPath = lib.escapeShellArg wrapperPath;
  in "wrapProgram \"$out/bin/gopass\" --prefix PATH : ${extraPath}";
}
