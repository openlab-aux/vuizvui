{ python3Packages, runCommand }:

python3Packages.buildPythonPackage {
  name = "aacolorize";
  pyproject = true;
  build-system = [ python3Packages.setuptools ];
  src = runCommand "aacolorize-src" {} ''
    mkdir -p "$out"
    cp "${./aacolorize.py}" "$out/aacolorize"
    cat > "$out/setup.py" <<SETUP
    from distutils.core import setup
    setup(name='aacolorize', scripts=['aacolorize'])
    SETUP
  '';
}
