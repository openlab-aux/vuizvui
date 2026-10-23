{ lib, ... }:

let
  # The machine has two PCIe sound cards with two 5.1 systems combined into a
  # single 10.2 system. Since both sound cards are identical models, we need to
  # match them via their corresponding PCIe slots and assign IDs.
  #
  # Note for reference, the default channels are: FL FR RL RR FC LFE
  cards = {
    lower = {
      description = "Lower soundcard";
      positions = [ "SL" "FR" "SR" "RL" "FLC" "LFE" ];
      devPath = "/devices/pci0000:20/0000:20:03.1/0000:23:00.0/*/sound/card?*";
    };
    upper = {
      description = "Upper soundcard";
      positions = [ "RC" "RR" "FC" "FL" "FRC" "LFE" ];
      devPath = "/devices/pci0000:40/0000:40:01.1/0000:41:00.0/*/sound/card?*";
    };
  };

  combinedPositions = let
    getPositions = lib.const (lib.getAttr "positions");
    positions = lib.concatLists (lib.mapAttrsToList getPositions cards);
  in lib.unique positions;

in {
  # This gives all the cards stable IDs and also ensures that they're not
  # auto-added to PipeWire so that we can initialise them with a custom channel
  # mapping.
  services.udev.extraRules = lib.concatMapAttrsStringSep "\n" (name: attrs: let
    common = lib.concatStringsSep ", " [
      "ACTION==\"add|change\""
      "DEVPATH==\"${attrs.devPath}\""
      "SUBSYSTEM==\"sound\""
    ];
  in ''
    ${common}, ATTR{id}!="${name}", ATTR{id}="${name}"
    ${common}, ENV{ACP_IGNORE}="1"
  '') cards;

  services.pipewire.extraConfig.pipewire."99-combine-cards" = {
    "context.objects" = lib.mapAttrsToList (name: attrs: {
      factory = "adapter";
      args = {
        "factory.name" = "api.alsa.pcm.sink";
        "node.name" = name;
        "node.description" = attrs.description;
        "media.class" = "Audio/Sink";
        "api.alsa.path" = "surround51:CARD=${name},DEV=0";
        "api.alsa.use-chmap" = false;
        "audio.rate" = 48000;
        "audio.channels" = lib.length attrs.positions;
        "audio.position" = attrs.positions;
      };
    }) cards;

    "context.modules" = lib.singleton {
      name = "libpipewire-module-combine-stream";
      args = {
        "combine.mode" = "sink";
        "node.name" = "combined";
        "node.description" = "Combined output to both soundcards";
        "combine.props" = {
          "media.class" = "Audio/Sink";
          "priority.session" = 10000;
          "audio.channels" = lib.length combinedPositions;
          "audio.position" = combinedPositions;
        };
        "stream.rules" = lib.mapAttrsToList (name: attrs: {
          matches = lib.singleton { "node.name" = name; };
          actions."create-stream"."audio.position" = attrs.positions;
        }) cards;
      };
    };
  };
}
