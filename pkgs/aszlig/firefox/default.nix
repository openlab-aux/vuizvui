{ lib, writeTextFile, writers, wrapFirefox, fetchFirefoxAddon
, firefox-unwrapped, tridactyl-native
}:

let
  mkExtension = name: { url, hash }: fetchFirefoxAddon {
    inherit name url hash;
  };

  extensions = lib.mapAttrs mkExtension (lib.importJSON ./addons.json);

  firefoxNoSigning = (firefox-unwrapped.override {
    crashreporterSupport = false;
    drmSupport = false;
    googleAPISupport = false;
  }).overrideAttrs (drv: {
    patches = (drv.patches or []) ++ [
      ./mute-by-default.patch
      ./dont-block-about-pages.patch
    ];
    MOZ_REQUIRE_SIGNING = false;
  });

  jsString = str: builtins.toJSON (toString str);

in wrapFirefox firefoxNoSigning {
  nixExtensions = lib.attrValues extensions;

  extraNativeMessagingHosts = [
    (writeTextFile {
      name = "ff2mpv-native";
      destination = "/lib/mozilla/native-messaging-hosts/ff2mpv.json";
      text = builtins.toJSON {
        name = "ff2mpv";
        description = "Helper to actually run mpv";
        path = let
          source = builtins.readFile ./ff2mpv.py;
        in writers.writePython3 "ff2mpv.py" {} source;
        type = "stdio";
        allowed_extensions = [ extensions.ff2mpv.extid ];
      };
    })
    (writeTextFile {
      name = "tridactyl-native";
      destination = "/lib/mozilla/native-messaging-hosts/tridactyl.json";
      text = builtins.toJSON {
        name = "tridactyl";
        description = "Tridactyl native command handler";
        path = "${tridactyl-native}/share/tridactyl/native_main.py";
        type = "stdio";
        allowed_extensions = [
          extensions.tridactyl-vim.extid
          "tridactyl.vim@cmcaine.co.uk"
        ];
      };
    })
  ];

  extraPrefs = ''
    lockPref('app.normandy.enabled', false);
    lockPref('app.normandy.first_run', false);
    lockPref('app.shield.optoutstudies.enabled', false);
    lockPref('browser.aboutConfig.showWarning', false);
    lockPref('browser.aboutwelcome.enabled', false);
    lockPref('browser.contentblocking.category', 'strict');
    lockPref('browser.laterrun.enabled', false);
    lockPref('browser.newtab.extensionControlled', true);
    lockPref('browser.newtab.privateAllowed', false);
    lockPref('browser.newtabpage.enabled', false);
    lockPref('browser.rights.3.shown', true);
    lockPref('browser.safebrowsing.malware.enabled', false);
    lockPref('browser.safebrowsing.phishing.enabled', false);
    lockPref('browser.shell.checkDefaultBrowser', false);
    lockPref('devtools.theme', 'dark');
    lockPref('extensions.webextensions.restrictedDomains', "");
    lockPref('privacy.donottrackheader.enabled', true);
    lockPref('privacy.query_stripping.enabled', true);
    lockPref('privacy.query_stripping.enabled.pbmode', true);
    lockPref('privacy.trackingprotection.enabled', true);
    lockPref('privacy.trackingprotection.socialtracking.enabled', true);
    lockPref('reader.color_scheme', 'dark');

    pref('browser.uiCustomization.state', ${jsString (builtins.toJSON {
      placements = {
        widget-overflow-fixed-list = [];
        nav-bar = [
          "back-button"
          "forward-button"
          "stop-reload-button"
          "urlbar-container"
          "downloads-button"
          "fxa-toolbar-menu-button"
          "nixos_ff2mpv-browser-action"
          "nixos_multi_account_containers-browser-action"
          "nixos_stylus-browser-action"
          "nixos_styl-us-browser-action"
          "nixos_multi-account-containers-browser-action"
        ];
        toolbar-menubar = [ "menubar-items" ];
        TabsToolbar = [ "tabbrowser-tabs" "new-tab-button" "alltabs-button" ];
        PersonalToolbar = [ "import-button" "personal-bookmarks" ];
      };
      currentVersion = 17;
    })});
  '';

  extraPolicies = {
    DisableFirefoxAccounts = true;
    DisableFirefoxStudies = true;
    DisablePocket = true;
    DisableSetDesktopBackground = true;
    DisableTelemetry = true;
    EnableTrackingProtection.Cryptomining = true;
    EnableTrackingProtection.Fingerprinting = true;
    EnableTrackingProtection.Locked = true;
    EnableTrackingProtection.Value = true;
    EncryptedMediaExtensions.Enabled = false;
    EncryptedMediaExtensions.Locked = true;
    FirefoxHome.Pocket = false;
    FirefoxHome.Snippets = false;
    OverrideFirstRunPage = "";
    UserMessaging.ExtensionRecommendations = false;
    UserMessaging.SkipOnboarding = false;
  };
}
