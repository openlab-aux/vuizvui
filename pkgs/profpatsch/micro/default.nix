# COPIED from nixpkgs for https://github.com/zyedidia/micro/pull/3868
{
  lib,
  buildGoModule,
  callPackage,
  fetchFromGitHub,
  installShellFiles,
  stdenv,
  # Deprecated options
  # Remove them as soon as possible
  withXclip ? null,
  withWlClipboard ? null,
  withWlclip ? null,
}:

let
  self = buildGoModule {
    pname = "micro";
    version = "2.0.14";

    src = fetchFromGitHub {
      owner = "zyedidia";
      repo = "micro";
      rev = "5b6eed51f8dff5a9fa00e581d4277e198231fedd";
      hash = "sha256-gr2OGziWTnEWg3qpYldsI1n3uu6NVLZIAG488DT+fDc=";
    };

    vendorHash = "sha256-UF5q7YJN1l19JlwhPWSDnIDsNNI1kHI5vGXTa9aII1E=";

    nativeBuildInputs = [ installShellFiles ];

    outputs = [
      "out"
      "man"
    ];

    subPackages = [ "cmd/micro" ];

    ldflags =
      let
        t = "github.com/zyedidia/micro/v2/internal";
      in
      [
        "-s"
        "-w"
        "-X ${t}/util.Version=${self.version}"
        "-X ${t}/util.CommitHash=${self.src.rev}"
      ];

    strictDeps = true;

    preBuild = ''
      GOOS= GOARCH= go generate ./runtime
    '';

    postInstall = ''
      installManPage assets/packaging/micro.1
      install -Dm444 assets/packaging/micro.desktop $out/share/applications/micro.desktop
      install -Dm644 assets/micro-logo-mark.svg $out/share/icons/hicolor/scalable/apps/micro.svg
    '';

    passthru = {
      tests = lib.packagesFromDirectoryRecursive {
        inherit callPackage;
        directory = ./tests;
      };
      wrapper = callPackage ./wrapper.nix { micro = self; };
    };

    meta = {
      homepage = "https://micro-editor.github.io";
      changelog = "https://github.com/zyedidia/micro/releases/";
      description = "Modern and intuitive terminal-based text editor";
      longDescription = ''
        micro is a terminal-based text editor that aims to be easy to use and
        intuitive, while also taking advantage of the capabilities of modern
        terminals.

        As its name indicates, micro aims to be somewhat of a successor to the
        nano editor by being easy to install and use. It strives to be enjoyable
        as a full-time editor for people who prefer to work in a terminal, or
        those who regularly edit files over SSH.
      '';
      license = lib.licenses.mit;
      mainProgram = "micro";
      maintainers = with lib.maintainers; [
        pbsds
      ];
    };
  };
in
lib.warnIf (withXclip != null || withWlClipboard != null || withWlclip != null) ''
  The options `withXclip`, `withWlClipboard`, `withWlclip` were removed. If
  you are seeking for clipboard support, please consider the following
  packages:
  - `micro-with-wl-clipboard`
  - `micro-with-xclip`
  - `micro-full`
'' self
