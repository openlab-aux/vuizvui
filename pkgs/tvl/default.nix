{ tvlSrc ? builtins.fetchGit {
    name = "tvl-depot";
    url = "https://code.tvl.fyi";
    rev = "ebd701b221a7df872b4514aa758c7e410fedc3c8"; # 2022-06-03
    ref = "canon";
  }
, pkgs
}:

import tvlSrc {
  nixpkgsBisectPath = pkgs.path; # TODO: does this improve eval time significantly?
}
