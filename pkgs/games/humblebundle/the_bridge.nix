{ buildUnity, fetchHumbleBundle
, libGLU
}:

buildUnity rec {
  fullName = "TheBridge";
  name = fullName;
  version = "20140908"; # 1410197597, or 1410196636 (same date).

  src = fetchHumbleBundle {
    name = "TheBridgeLinux_1410196636.zip";
    machineName = "thebridge_linux";
    downloadName = "Download";
    md5 = "6d3f5e7ff8d10d47f04ffabb8b9a031e";
  };

  buildInputs = [ libGLU ];

  meta = {
    homepage = [
      http://thebridgeisblackandwhite.com
      https://www.humblebundle.com/store/the-bridge
    ];
    #editor = "The Quantum Astrophysicists Guild";
    description = "A 2D logic puzzle game that plays with physics and perspective";
    longDescription = ''
      The Bridge is a 2D logic puzzle game that forces the player to reevaluate
      their preconceptions of physics and perspective. It is Isaac Newton meets
      M. C. Escher. Manipulate gravity to redefine the ceiling as the floor
      while venturing through impossible architectures. Explore increasingly
      difficult worlds, each uniquely detailed and designed to leave the player
      with a pronounced sense of intellectual accomplishment. The Bridge
      exemplifies games as an art form, with beautifully hand-drawn art in the
      style of a black-and-white lithograph.
    '';
  };
}
