{ buildRustCrate }:

let
  libc = buildRustCrate {
    pname = "libc";
    version = "0.2.69";
    crateName = "libc";
    sha256 = "0fwi6rxklsaqcig432fg3cjamiilvv2c4jz0i3dxw7c33ipprhsz";
  };

  errno = buildRustCrate {
    pname = "errno";
    version = "0.2.5";
    crateName = "errno";
    sha256 = "0gd36jijlb17df3ffxqxqczlwdawicbbzqjwfjc4b5lzqgizm0bz";
    dependencies = [ libc ];
  };

  serde = buildRustCrate {
    pname = "serde";
    crateName = "serde";
    version = "1.0.123";
    edition = "2015";
    sha256 = "05xl2s1vpf3p7fi2yc9qlzw88d5ap0z3qmhmd7axa6pp9pn1s5xc";
    features = [ "std" ];
  };

  cfg-if = buildRustCrate {
    pname = "cfg-if";
    crateName = "cfg-if";
    version = "1.0.0";
    sha256 = "1fzidq152hnxhg4lj6r2gv4jpnn8yivp27z6q6xy7w6v0dp6bai9";
  };

  log = buildRustCrate {
    pname = "log";
    version = "0.4.11";
    crateName = "log";
    sha256 = "0m6xhqxsps5mgd7r91g5mqkndbh8zbjd58p7w75r330zl4n40l07";
    dependencies = [ cfg-if ];
  };

  mustache = buildRustCrate {
    pname = "mustache";
    version = "0.9.0";
    crateName = "mustache";
    edition = "2015";
    sha256 = "1zgl8l15i19lzp90icgwyi6zqdd31b9vm8w129f41d1zd0hs7ayq";
    dependencies = [ log serde ];
  };

  toml = buildRustCrate {
    pname = "toml";
    version = "0.5.8";
    crateName = "toml";
    sha256 = "1vwjwmwsy83pbgvvm11a6grbhb09zkcrv9v95wfwv48wjm01wdj4";
    edition = "2018";
    dependencies = [ serde ];
  };

  regex-syntax = buildRustCrate {
    pname = "regex-syntax";
    crateName = "regex-syntax";
    version = "0.6.22";
    edition = "2015";
    sha256 = "0r00n2dgyixacl1sczqp18gxf0xh7x272hcdp62412lypba2gqyg";
  };

  regex = buildRustCrate {
    pname = "regex";
    crateName = "regex";
    version = "1.4.3";
    features = [ "std" ];
    dependencies = [ regex-syntax ];
    edition = "2015";
    sha256 = "0w0b4bh0ng20lf5y8raaxmxj46ikjqpgwy1iggzpby9lhv9vydkp";
  };

  lazy_static = buildRustCrate {
    pname = "lazy_static";
    version = "1.4.0";
    crateName = "lazy_static";
    sha256 = "13h6sdghdcy7vcqsm2gasfw3qg7ssa0fl3sw7lq6pdkbk52wbyfr";
  };

in {
  inherit
    libc
    errno
    mustache
    toml
    regex
    lazy_static
    ;
}
