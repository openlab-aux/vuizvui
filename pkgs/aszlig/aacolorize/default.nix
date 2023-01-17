{ python3Packages, runCommand }:

python3Packages.buildPythonPackage {
  name = "aacolorize";
  src = runCommand "aacolorize-src" {} ''
    mkdir -p "$out"
    cp "${./aacolorize.py}" "$out/aacolorize"
    cat > "$out/setup.py" <<SETUP
    from distutils.core import setup
    setup(name='aacolorize', scripts=['aacolorize'])
    SETUP
  '';
}
