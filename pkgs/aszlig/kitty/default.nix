{ kitty, ruff, clang-tools }:

kitty.overridePythonAttrs (drv: {
  patches = (drv.patches or []) ++ [ ./config.patch ];

  # Needed by gen/config.py.
  nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [
    ruff clang-tools
  ];

  # This is needed to regenerate various files in kitty/options/ so that the
  # changes in kitty/options/definitions are actually reflected in the real
  # defaults. Since generating those files needs a whole bunch of Python
  # packages and other magic, we build Kitty here, generate the config and
  # then we build it again with the newly generated files.
  buildPhase = ''
    ${drv.buildPhase}
    # This script is called by gen/config.py.
    patchShebangs autoformat

    # The autoformat scripts tries to clang-format all the files in the
    # vendor directory, so we need write permissions.
    chmod -R u+w vendor

    gen/config.py

    # The fonts directory is created in the first invocation of buildPhase
    # using "mkdir" (without any flags), which will fail during the second
    # invocation.
    rm -r fonts
    ${drv.buildPhase}
  '';
})
