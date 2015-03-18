{ buildGoPackage, lib, fetchFromGitHub, fetchgit, fetchhg }:

rec {
  cascadia = buildGoPackage {
    name = "cascadia";
    goPackagePath = "code.google.com/p/cascadia";
    src = fetchhg {
      url = "https://code.google.com/p/cascadia";
      rev = "5d796540e3cb93ea0556c897e6a3c7690f614d35";
      sha256 = "1mxmj4vbh47j3nvmdqdah4fprkyww3pf8i9sy0zcar52bpa4j69c";
    };
    buildInputs = [ net ];
  };

  charset = buildGoPackage {
    name = "go-charset";
    goPackagePath = "code.google.com/p/go-charset";
    src = fetchhg {
      url = "https://code.google.com/p/go-charset";
      rev = "ebbeafdc430eb6c7e44e9a730a38eaff4c56ba3a";
      sha256 = "162jd0ryvwaj7bwxbdwrs1vi6ig3bhd6m4n16wf54frrzyqxh34p";
    };
  };

  gonet = buildGoPackage {
    name = "go.net";
    goPackagePath = "code.google.com/p/go.net";
    src = fetchhg {
      url = "https://code.google.com/p/go.net";
      rev = "937a34c9de13c766c814510f76bca091dee06028";
      sha256 = "1f91yzjllw2pdk68yjvf8hjix4mrlqn7fh97h9n7qjy903rwnb9q";
    };
    buildInputs = [ net text ];
  };

  gomock = buildGoPackage {
    name = "gomock";
    goPackagePath = "code.google.com/p/gomock";
    src = fetchgit {
      url = "https://code.google.com/p/gomock";
      rev = "e033c7513ca3d743bbb64df299bdec29e93fed03";
      sha256 = "0vmpqibyx09bdqnqsy8g4xiw3hpw0j9kww7ak2z7fdzxpd9ly337";
    };
  };

  anaconda = buildGoPackage {
    name = "anaconda";
    goPackagePath = "github.com/ChimeraCoder/anaconda";
    src = fetchFromGitHub {
      owner = "ChimeraCoder";
      repo = "anaconda";
      rev = "964821c05001e5a38dd234d681ce9a929858481a";
      sha256 = "077fxb4iazsjfsbmj966ifias84agxbzip742w9cbc7fv1bpy085";
    };
    buildInputs = [ tokenbucket jsonpointer oauth ];
  };

  tokenbucket = buildGoPackage {
    name = "tokenbucket";
    goPackagePath = "github.com/ChimeraCoder/tokenbucket";
    src = fetchFromGitHub {
      owner = "ChimeraCoder";
      repo = "tokenbucket";
      rev = "c5a927568de7aad8a58127d80bcd36ca4e71e454";
      sha256 = "1cyzlvk1mgdvdfmqsdsy5y2rflfz5q54a9rz9jylc2mg40c1d6dq";
    };
  };

  gotumblr = buildGoPackage {
    name = "gotumblr";
    goPackagePath = "github.com/MariaTerzieva/gotumblr";
    src = fetchFromGitHub {
      owner = "MariaTerzieva";
      repo = "gotumblr";
      rev = "62f45d64049aeab0b3835351edc66704c7210f7a";
      sha256 = "06bqc6c4j9g8l0xqhc9g5jmx4q6dq5jid5bpj4skca30gsqgldgr";
    };
    buildInputs = [ oauth1a ];
  };

  goquery = buildGoPackage {
    name = "goquery";
    goPackagePath = "github.com/PuerkitoBio/goquery";
    src = fetchFromGitHub {
      owner = "PuerkitoBio";
      repo = "goquery";
      rev = "4cf64c51f7e80d56d9ae2ffe7d684d3dd5dbd5d0";
      sha256 = "1d6cl0qhfx9ngj3hn56mxwwy7yak62c5wxa77f7yfarql84r8h4n";
    };
    buildInputs = [ cascadia net ];
  };

  GoOse = buildGoPackage {
    name = "GoOse";
    goPackagePath = "github.com/advancedlogic/GoOse";
    src = fetchFromGitHub {
      owner = "advancedlogic";
      repo = "GoOse";
      rev = "e210b2436fec0a3ce1b5f9209ee3340314b408e2";
      sha256 = "0sjqy295x9rn93b5k3r8hdbi5gjbdd3h2dn89v4nzpnzmlrfbc2c";
    };
    buildInputs = [ cascadia charset gojs-config goquery net latinx set ];
  };

  gojs-config = buildGoPackage {
    name = "gojs-config";
    goPackagePath = "github.com/advancedlogic/gojs-config";
    src = fetchFromGitHub {
      owner = "advancedlogic";
      repo = "gojs-config";
      rev = "bff36193fca8bd2f6269e8c4e8c723991fd20565";
      sha256 = "1k0wgn3pj384sqai2c9dkv06j0z439i3xqzfl3kplb0wdf8a2vy0";
    };
  };

  latinx = buildGoPackage {
    name = "latinx";
    goPackagePath = "github.com/bjarneh/latinx";
    src = fetchFromGitHub {
      owner = "bjarneh";
      repo = "latinx";
      rev = "4dfe9ba2a293f28a5e06fc7ffe56b1d71a47b8c8";
      sha256 = "0lavz5m0dz1rxyl20var3xqj2ndcmai2v893p83pjwm4333yb5g0";
    };
  };

  jsonpointer = buildGoPackage {
    name = "go-jsonpointer";
    goPackagePath = "github.com/dustin/go-jsonpointer";
    src = fetchFromGitHub {
      owner = "dustin";
      repo = "go-jsonpointer";
      rev = "75939f54b39e7dafae879e61f65438dadc5f288c";
      sha256 = "1vcv5xb6v6akbbi71q4srfla311s4p9kspqya2h40x8fxx00lkxp";
    };
    propagatedBuildInputs = [ gojson ];
  };

  gojson = buildGoPackage {
    name = "gojson";
    goPackagePath = "github.com/dustin/gojson";
    src = fetchFromGitHub {
      owner = "dustin";
      repo = "gojson";
      rev = "af16e0e771e2ed110f2785564ae33931de8829e4";
      sha256 = "0626n6a5hwb0zwi6dwsmqdv2g5fwzsfx22rbxscaydpb90b6qnin";
    };
  };

  restful = buildGoPackage {
    name = "go-restful";
    goPackagePath = "github.com/emicklei/go-restful";
    src = fetchFromGitHub {
      owner = "emicklei";
      repo = "go-restful";
      rev = "7ef8ec372029a3112fdb94a53b1ca8eedf666e67";
      sha256 = "0rrpa9xiqkzapn6axjl19nnhxk0ljjq20a8jpam80hkzw4waa955";
    };
    postPatch = ''
      rm -rf examples
    '';
    buildInputs = [ schema ];
  };

  goirc = buildGoPackage {
    name = "goirc";
    goPackagePath = "github.com/fluffle/goirc";
    src = fetchFromGitHub {
      owner = "fluffle";
      repo = "goirc";
      rev = "0cac69d2eec69bb08bb29b776d045a78b9699791";
      sha256 = "0iba19rslsyww3qsf9d4ncdxjjz7pv8k36ar5s1i6f4fwv42d56q";
    };
    buildInputs = [ glog golog gomock ];
  };

  golog = buildGoPackage {
    name = "golog";
    goPackagePath = "github.com/fluffle/golog";
    src = fetchFromGitHub {
      owner = "fluffle";
      repo = "golog";
      rev = "3b86dae249b53d7dc2d9e817ff019fa01a155b06";
      sha256 = "0b8fzkk9bshkfsnbx2nq6dn0dcngsh5awpym98sinkkfwywvlq2f";
    };
    buildInputs = [ gomock ];
  };

  oauth = buildGoPackage {
    name = "go-oauth";
    goPackagePath = "github.com/garyburd/go-oauth";
    src = fetchFromGitHub {
      owner = "garyburd";
      repo = "go-oauth";
      rev = "fa02955a8929c2f007c533fbdfb8ddc91bb6a731";
      sha256 = "0zx9azdhjxf18fk4y3hnp70cz75iyllqfvfxma02i8f63q364d94";
    };
    postPatch = ''
      rm -rf examples
    '';
  };

  glog = buildGoPackage {
    name = "glog";
    goPackagePath = "github.com/golang/glog";
    src = fetchFromGitHub {
      owner = "golang";
      repo = "glog";
      rev = "44145f04b68cf362d9c4df2182967c2275eaefed";
      sha256 = "1k7sf6qmpgm0iw81gx2dwggf9di6lgw0n54mni7862hihwfrb5rq";
    };
  };

  protobuf = buildGoPackage {
    name = "protobuf";
    goPackagePath = "github.com/golang/protobuf";
    src = fetchFromGitHub {
      owner = "golang";
      repo = "protobuf";
      rev = "f7137ae6b19afbfd61a94b746fda3b3fe0491874";
      sha256 = "05n1ws6y9qpp3imxjvl3jnknq6kca2vc5g475fqr2l67ap3w5lwk";
    };
    subPackages = [ "proto" "protoc-gen-go" ];
  };

  schema = buildGoPackage {
    name = "schema";
    goPackagePath = "github.com/gorilla/schema";
    src = fetchFromGitHub {
      owner = "gorilla";
      repo = "schema";
      rev = "c8422571edf3131506bab7df27e18980fe2598d5";
      sha256 = "10czpd111l834aam52bh1cxv31pq4h8mi1w994v4848rmbw3jpp4";
    };
  };

  dbus = buildGoPackage {
    name = "go.dbus";
    goPackagePath = "github.com/guelfey/go.dbus";
    src = fetchFromGitHub {
      owner = "guelfey";
      repo = "go.dbus";
      rev = "f6a3a2366cc39b8479cadc499d3c735fb10fbdda";
      sha256 = "15rnpvclg4b3cblcxwwgkdfgamhigiyla0s1rwhfjraqhn94r3ph";
    };
    postPatch = ''
      rm -rf _examples
    '';
  };

  web = buildGoPackage {
    name = "web";
    goPackagePath = "github.com/hoisie/web";
    src = fetchFromGitHub {
      owner = "hoisie";
      repo = "web";
      rev = "5a66d0fa07a54688eba8fa506576a78a942ef243";
      sha256 = "1h4ary4ac51xznr41996k3xqlclm3r5mjba71y6anfwdrhaa2qf1";
    };
    buildInputs = [ gonet ];
    postPatch = ''
      rm -rf examples
    '';
  };

  goserial = buildGoPackage {
    name = "goserial";
    goPackagePath = "github.com/huin/goserial";
    src = fetchFromGitHub {
      owner = "huin";
      repo = "goserial";
      rev = "7b90efdb22b1c168a57b998b2780cf541b2c4740";
      sha256 = "05ha3yvhvbfrbxlqi8x1fwcliginw0vxhh76mh6vycn9n7yjpacy";
    };
  };

  rss = buildGoPackage {
    name = "go-pkg-rss";
    goPackagePath = "github.com/jteeuwen/go-pkg-rss";
    src = fetchFromGitHub {
      owner = "jteeuwen";
      repo = "go-pkg-rss";
      rev = "2382fc0262cb000be19e9042cdbc8459105b4f00";
      sha256 = "0rss5sj128qwai60wpkm5cy2q8d9yfakdm4pqb8p4lhgpq26g05h";
    };
    buildInputs = [ xmlx ];
  };

  xmlx = buildGoPackage {
    name = "go-pkg-xmlx";
    goPackagePath = "github.com/jteeuwen/go-pkg-xmlx";
    src = fetchFromGitHub {
      owner = "jteeuwen";
      repo = "go-pkg-xmlx";
      rev = "cf505b97c711dd1c5a4682f68ea04dd35e385b8f";
      sha256 = "01pdjndl1i0p7lr8svi1j0f79zyl36s0xn7yb8d8yziksbczbcrj";
    };
  };

  oauth1a = buildGoPackage {
    name = "oauth1a";
    goPackagePath = "github.com/kurrik/oauth1a";
    src = fetchFromGitHub {
      owner = "kurrik";
      repo = "oauth1a";
      rev = "fc2542bc5f2532ed4a437960d2d51ff6e18a5cb6";
      sha256 = "1v9zsn80y5x5fklc7q8rxixjrh5g01rsdlz247lgf3rag0hb3d39";
    };
  };

  xmpp = buildGoPackage {
    name = "go-xmpp";
    goPackagePath = "github.com/mattn/go-xmpp";
    src = fetchFromGitHub {
      owner = "mattn";
      repo = "go-xmpp";
      rev = "8b13d0ad771420685f85ed09d8e9bf81757e7e20";
      sha256 = "022all0cphxmrg015jzfsqd5xd5nli7fpw32wx6ql79s4rsy3bwb";
    };
    postPatch = ''
      rm -rf _example
    '';
  };

  goefa = buildGoPackage {
    name = "goefa";
    goPackagePath = "github.com/michiwend/goefa";
    src = fetchFromGitHub {
      owner = "michiwend";
      repo = "goefa";
      rev = "381f3d7b77fc04d9a81d2bc9e3e6d2fc742757b1";
      sha256 = "1aiiafbpvw2xlvjgh27mfljd3d0j443iz7sp9w9w3109ay1q2gk4";
    };
    buildInputs = [ charset ];
  };

  hue = buildGoPackage {
    name = "go.hue";
    goPackagePath = "github.com/muesli/go.hue";
    src = fetchFromGitHub {
      owner = "muesli";
      repo = "go.hue";
      rev = "8aefcc693cafb5b2b4ef8ca8d51ab880849e8c12";
      sha256 = "158q3g5rg9wra1wxkvyb1c2v868gp9mslhf6gmbifj516lsb1agi";
    };
  };

  net = buildGoPackage {
    name = "net";
    goPackagePath = "golang.org/x/net";
    src = fetchgit {
      url = "https://go.googlesource.com/net";
      rev = "97d8e4e174133a4d1d2171380e510eb4dea8f5ea";
      sha256 = "0jydngilxhgw8f1zgz11hbjk87bhj0jpar89a2py1pii4ncx9w04";
    };
    buildInputs = [ text ];
  };

  text = buildGoPackage {
    name = "text";
    goPackagePath = "golang.org/x/text";
    src = fetchgit {
      url = "https://go.googlesource.com/text";
      rev = "26df76be81cdb060ed9820481a0d67b2d0b04ac2";
      sha256 = "1vmgzzi0r1idjfgfwibq2r3xlnab3w2v6nmm3c5l2bb994w376gn";
    };
  };

  set = buildGoPackage {
    name = "set.v0";
    goPackagePath = "gopkg.in/fatih/set.v0";
    src = fetchgit {
      url = "https://gopkg.in/fatih/set.v0";
      rev = "27c40922c40b43fe04554d8223a402af3ea333f3";
      sha256 = "1d8yz8p4jvyqvmpim40x5y7kj91c5hcc5hbmxhv0j32ifz01nacl";
    };
  };
}
