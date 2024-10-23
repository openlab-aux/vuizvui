{ python3Packages, libgudev, fetchFromGitHub, wrapGAppsHook3, gobject-introspection, scdoc }:

python3Packages.buildPythonApplication rec {
    pname = "blight";
    version = "1.0.0";

    src = fetchFromGitHub {
        owner = "rpigott";
        repo = "blight";
        rev = "312c70472029a67ddbe2da375f1e9596aa5c8d61";
        sha256 = "sha256-OutGJFNSr25OXjrE/jGCnJ1GMNdGJV1mfU5bPeJHcg8=";
    };

    format = "other";
    dontBuild = true;

    dependencies = [
        python3Packages.ipython
        python3Packages.pygobject3
    ];

    nativeBuildInputs = [
        wrapGAppsHook3
        gobject-introspection
        scdoc
    ];

    propagatedBuildInputs = [
        libgudev
    ];

    # install manpages from the completion/bash and completion/zsh dirs
    installPhase = ''
        # copy blight.py
        mkdir -p $out/bin
        install -Dm755 blight.py $out/bin/blight

        # build manpage with scdoc
        mkdir -p $out/share/man/man1
        scdoc < blight.1.scd > $out/share/man/man1/blight.1

        cp -r completion $out/share
    '';

    doCheck = false;
}
