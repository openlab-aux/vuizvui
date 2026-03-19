# COPIED from nixpkgs for https://github.com/zyedidia/micro/pull/3868
{
  lib,
  pkgs,
  # Deprecated options
  # Remove them as soon as possible
  withXclip ? null,
  withWlClipboard ? null,
  withWlclip ? null,
  ...
}:

let
  self = pkgs.buildGoModule {
    pname = "micro";
    version = "2.0.15";

    src = pkgs.fetchFromGitHub {
      owner = "zyedidia";
      repo = "micro";
      rev = "v2.0.15";
      hash = "sha256-4C6TtMU6PIYX7lO+o4GRVnIsKnYJxjAqPdoOyAwi7Gc=";
    };

    vendorHash = "sha256-Pn3C/Te47qXeqgB9UPynB8aPtnwnjrqq/ZE9bA7JRVs=";

    nativeBuildInputs = [ pkgs.installShellFiles ];

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
        callPackage = pkgs.callPackage;
        directory = ./tests;
      };
      wrapper = pkgs.callPackage ./wrapper.nix { micro = self; };
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
