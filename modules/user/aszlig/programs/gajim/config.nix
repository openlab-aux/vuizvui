lib: with lib;

let
  mkConfig = let
    traverse = path: attrs: let
      mkVal = name: value: let
        flatPath = concatStringsSep "." (path ++ [name]);
      in if isAttrs value then traverse (path ++ [name]) value
         else if value == true then "${flatPath} = True"
         else if value == false then "${flatPath} = False"
         else "${flatPath} = ${value}";
    in concatStringsSep "\n" (mapAttrsToList mkVal attrs);

    rootTraverse = attrs: (traverse [] attrs) + "\n";
  in rootTraverse;

in mkConfig {
  activity_iconset = "default";
  after_nickname = ">";
  allow_hide_roster = true;
  always_english_wikipedia = false;
  always_english_wiktionary = true;
  ascii_formatting = true;
  ask_avatars_on_startup = true;
  ask_offline_status = true;
  ask_offline_status_on_connection = false;
  ask_online_status = false;
  attach_notifications_to_systray = false;
  audio_input_device = "pulsesrc device=alsa_input."
                     + "usb-046d_0804_DD519390-02-U0x46d0x804.analog-mono"
                     + " ! volume name=gajim_vol";
  audio_input_volume = "50";
  audio_output_device = "pulsesink device=alsa_output."
                      + "pci-0000_00_1b.0.analog-stereo sync=true";
  audio_output_volume = "50";
  autoaway = false;
  autoaway_message = "$S (Away as a result of being idle more than $T min)";
  autoawaytime = "5";
  autodetect_browser_mailer = false;
  autopopup = true;
  autopopupaway = true;
  autoxa = false;
  autoxa_message = "$S (Not available as a result of being"
                 + " idle more than $T min)";
  autoxatime = "15";
  avatar_position_in_roster = "left";
  before_nickname = "<";
  change_roster_title = true;
  change_status_window_timeout = "15";
  "chat-msgwin-height" = "440";
  "chat-msgwin-width" = "480";
  "chat-msgwin-x-position" = "-1";
  "chat-msgwin-y-position" = "-1";
  chat_avatar_height = "52";
  chat_avatar_width = "52";
  chat_merge_consecutive_nickname = false;
  chat_merge_consecutive_nickname_indent = "  ";
  check_idle_every_foo_seconds = "2";
  check_if_gajim_is_default = true;
  collapsed_rows = "";
  compact_view = false;
  confirm_block = "";
  confirm_close_muc = true;
  confirm_close_muc_rooms = "";
  confirm_close_multiple_tabs = true;
  confirm_custom_status = "no";
  confirm_metacontacts = "no";
  conversation_font = "Liberation Mono 10";
  ctrl_tab_go_to_next_composing = true;
  custom_file_manager = "";
  custombrowser = "chromium";
  custommailapp = "";
  dictionary_url = "WIKTIONARY";
  displayed_chat_state_notifications = "all";
  emoticons_theme = "";
  enable_negative_priority = false;
  escape_key_closes = false;
  esession_modp = "5,14";
  file_transfers_port = "28011";
  ft_add_hosts_to_send = "";
  "gc-hpaned-position" = "979";
  "gc-msgwin-height" = "440";
  "gc-msgwin-width" = "600";
  "gc-msgwin-x-position" = "-1";
  "gc-msgwin-y-position" = "-1";
  gc_nicknames_colors = "#4e9a06:#f57900:#ce5c00:#3465a4:#204a87:#75507b:"
                      + "#5c3566:#c17d11:#8f5902:#ef2929:#cc0000:#a40000";
  gc_proposed_nick_char = "_";
  gc_refer_to_nick_char = ":";
  global_proxy = "";
  hide_avatar_of_transport = true;
  hide_chat_banner = false;
  hide_groupchat_banner = false;
  hide_groupchat_occupants_list = false;
  history_window_height = "1156";
  history_window_width = "1596";
  "history_window_x-position" = "0";
  "history_window_y-position" = "20";
  iconset = "dcraven";
  ignore_incoming_xhtml = false;
  inmsgcolor = "#ff7f50";
  inmsgfont = "";
  inmsgtxtcolor = "";
  inmsgtxtfont = "";
  just_connected_bg_color = "#adc3c6";
  just_disconnected_bg_color = "#ab6161";
  key_up_lines = "25";
  last_emoticons_dir = "";
  last_roster_visible = true;
  last_save_dir = "";
  last_send_dir = "";
  last_sounds_dir = "";
  latex_png_dpi = "108";
  log_contact_status_changes = true;
  log_xhtml_messages = false;
  markedmsgcolor = "#ff8080";
  max_conversation_lines = "500";
  mergeaccounts = false;
  mood_iconset = "default";
  "msgwin-height" = "1156";
  "msgwin-max-state" = true;
  "msgwin-width" = "1336";
  "msgwin-x-position" = "0";
  "msgwin-y-position" = "20";
  muc_autorejoin_on_kick = false;
  muc_autorejoin_timeout = "1";
  muc_highlight_words = "DOWN;PROBLEM;CRITICAL;UNREACHABLE";
  muc_restore_lines = "20";
  muc_restore_timeout = "60";
  networkmanager_support = true;
  noconfirm_close_muc_rooms = "";
  notification_avatar_height = "48";
  notification_avatar_width = "48";
  notification_position_x = "-1";
  notification_position_y = "-1";
  notification_preview_message = true;
  notification_timeout = "5";
  notify_on_all_muc_messages = false;
  notify_on_file_complete = true;
  notify_on_new_gmail_email = true;
  notify_on_new_gmail_email_command = "";
  notify_on_new_gmail_email_extra = false;
  notify_on_new_message = false;
  notify_on_signin = false;
  notify_on_signout = false;
  one_message_window = "always_with_roster";
  openwith = "xdg-open";
  outgoing_chat_state_notifications = "composing_only";
  outmsgcolor = "#add8e6";
  outmsgfont = "";
  outmsgtxtcolor = "";
  outmsgtxtfont = "";
  plugins.plugin_installer.active = false;
  print_ichat_every_foo_minutes = "5";
  print_status_in_chats = true;
  print_status_in_muc = "in_and_out";
  print_time = "always";
  print_time_fuzzy = "0";
  quit_on_roster_x_button = true;
  recently_groupchat = "";
  remote_control = true;
  restore_lines = "10";
  restore_timeout = "60";
  restored_messages_color = "#555753";
  restored_messages_small = false;
  roster_avatar_height = "16";
  roster_avatar_width = "16";
  roster_height = "1156";
  roster_theme = "blue";
  roster_width = "206";
  roster_window_skip_taskbar = false;
  "roster_x-position" = "0";
  "roster_y-position" = "20";
  rst_formatting_outgoing_messages = false;
  "save-roster-position" = true;
  scroll_roster_to_last_message = true;
  search_engine = "https://www.google.com/search?&q=%s&sourceid=gajim";
  send_on_ctrl_enter = false;
  send_sha_in_gc_presence = true;
  shell_like_completion = true;
  show_activity_in_roster = true;
  show_affiliation_in_groupchat = true;
  show_ascii_formatting_chars = true;
  show_avatar_in_chat = true;
  show_avatars_in_roster = true;
  show_contacts_number = true;
  show_location_in_roster = true;
  show_mood_in_roster = true;
  show_only_chat_and_online = false;
  show_roster_on_startup = "always";
  show_self_contact = "when_other_resource";
  show_status_msgs_in_roster = true;
  show_transports_group = true;
  show_tunes_in_roster = true;
  show_unread_tab_icon = true;
  showoffline = false;
  "single-msg-height" = "280";
  "single-msg-width" = "400";
  "single-msg-x-position" = "0";
  "single-msg-y-position" = "0";
  sort_by_show_in_muc = false;
  sort_by_show_in_roster = true;
  sounddnd = false;
  soundplayer = "aplay -q";
  sounds_on = false;
  speller_language = "de.en";
  statusmsgcolor = "#4e9a06";
  statusmsgfont = "";
  stun_server = "";
  tabs_always_visible = true;
  tabs_border = false;
  tabs_close_button = true;
  tabs_position = "right";
  time_stamp = "[%H:%M:%S]";
  tooltip_account_name_color = "#888A85";
  tooltip_affiliation_administrator_color = "#F57900";
  tooltip_affiliation_member_color = "#73D216";
  tooltip_affiliation_none_color = "#555753";
  tooltip_affiliation_owner_color = "#CC0000";
  tooltip_avatar_height = "125";
  tooltip_avatar_width = "125";
  tooltip_idle_color = "#888A85";
  tooltip_status_away_color = "#EDD400";
  tooltip_status_busy_color = "#F57900";
  tooltip_status_free_for_chat_color = "#3465A4";
  tooltip_status_na_color = "#CC0000";
  tooltip_status_offline_color = "#555753";
  tooltip_status_online_color = "#73D216";
  trayicon = "never";
  trayicon_notification_on_events = true;
  treat_incoming_messages = "";
  uri_schemes = "aaa:// aaas:// acap:// cap:// cid: crid:// data: dav: "
              + "dict:// dns: fax: file:/ ftp:// geo: go: gopher:// h323: "
              + "http:// https:// iax: icap:// im: imap:// info: ipp:// iris: "
              + "iris.beep: iris.xpc: iris.xpcs: iris.lwz: ldap:// mid: "
              + "modem: msrp:// msrps:// mtqp:// mupdate:// news: nfs:// "
              + "nntp:// opaquelocktoken: pop:// pres: prospero:// rtsp:// "
              + "service: shttp:// sip: sips: sms: snmp:// soap.beep:// "
              + "soap.beeps:// tag: tel: telnet:// tftp:// thismessage:/ "
              + "tip:// tv: urn:// vemmi:// xmlrpc.beep:// xmlrpc.beeps:// "
              + "z39.50r:// z39.50s:// about: apt: cvs:// daap:// ed2k:// "
              + "feed: fish:// git:// iax2: irc:// ircs:// ldaps:// magnet: "
              + "mms:// rsync:// ssh:// svn:// sftp:// smb:// webcal://";
  urlmsgcolor = "#add8e6";
  use_gnomekeyring = true;
  use_gpg_agent = true;
  use_kib_mib = false;
  use_kwalletcli = true;
  use_latex = false;
  use_notif_daemon = true;
  use_smooth_scrolling = true;
  use_speller = true;
  use_stun_server = false;
  use_transports_iconsets = true;
  use_urgency_hint = true;
  vcard_avatar_height = "200";
  vcard_avatar_width = "200";
  verbose = false;
  version = "0.15.4";
  video_framerate = "";
  video_input_device = "v4l2src device=/dev/video0";
  video_output_device = "ximagesink";
  video_size = "";

  accounts = {
    Local = {
      action_when_plaintext_connection = "warn";
      active = true;
      adjust_priority_with_status = true;
      allow_no_log_for = "";
      anonymous_auth = false;
      answer_receipts = true;
      attached_gpg_keys = "";
      autoauth = false;
      autoconnect = true;
      autoconnect_as = "online";
      autonegotiate_esessions = true;
      autopriority_away = "40";
      autopriority_chat = "50";
      autopriority_dnd = "20";
      autopriority_invisible = "10";
      autopriority_online = "50";
      autopriority_xa = "30";
      autoreconnect = true;
      client_cert = "";
      client_cert_encrypted = false;
      connection_types = "tls ssl plain";
      custom_host = "";
      custom_port = "5298";
      dont_ack_subscription = false;
      enable_esessions = true;
      enable_message_carbons = false;
      file_transfer_proxies = "proxy.eu.jabber.org, proxy.jabber.ru, "
                            + "proxy.jabbim.cz";
      ft_send_local_ips = true;
      gpg_sign_presence = true;
      hostname = "mmrnmhrm";
      http_auth = "ask";
      ignore_ssl_errors = "";
      ignore_unknown_contacts = false;
      is_zeroconf = true;
      keep_alive_every_foo_secs = "55";
      keep_alives_enabled = true;
      keyid = "";
      keyname = "";
      last_archiving_time = "1970-01-01T00:00:00Z";
      last_status = "online";
      last_status_msg = "";
      listen_to_network_manager = true;
      log_encrypted_sessions = true;
      minimized_gc = "";
      "msgwin-height" = "440";
      "msgwin-width" = "480";
      "msgwin-x-position" = "-1";
      "msgwin-y-position" = "-1";
      name = "aszlig";
      no_log_for = "";
      password = "zeroconf";
      ping_alive_every_foo_secs = "120";
      ping_alives_enabled = true;
      priority = "5";
      proxy = "";
      publish_location = false;
      publish_tune = false;
      request_receipt = true;
      resource = "gajim";
      restore_last_status = false;
      roster_version = "";
      savepass = false;
      send_idle_time = true;
      send_os_info = true;
      send_time_info = true;
      ssl_fingerprint_sha1 = "";
      subscribe_activity = true;
      subscribe_location = true;
      subscribe_mood = true;
      subscribe_nick = true;
      subscribe_tune = true;
      subscription_request_msg = "";
      sync_with_global_status = true;
      test_ft_proxies_on_startup = true;
      time_for_ping_alive_answer = "60";
      try_connecting_for_foo_secs = "60";
      use_custom_host = false;
      use_env_http_proxy = false;
      use_ft_proxies = false;
      use_srv = true;
      warn_when_insecure_password = true;
      warn_when_insecure_ssl_connection = true;
      zeroconf_email = "";
      zeroconf_first_name = "";
      zeroconf_jabber_id = "";
      zeroconf_last_name = "";
    };

    "aszlig.net" = {
      action_when_plaintext_connection = "disconnect";
      active = true;
      adjust_priority_with_status = true;
      allow_no_log_for = "";
      anonymous_auth = false;
      answer_receipts = true;
      autoauth = false;
      autoconnect = false;
      autoconnect_as = "online";
      autonegotiate_esessions = true;
      autopriority_away = "40";
      autopriority_chat = "50";
      autopriority_dnd = "20";
      autopriority_invisible = "10";
      autopriority_online = "50";
      autopriority_xa = "30";
      autoreconnect = true;
      client_cert = "";
      client_cert_encrypted = false;
      connection_types = "tls ssl plain";
      custom_host = "aszlig.net";
      custom_port = "5222";
      dont_ack_subscription = false;
      enable_esessions = true;
      enable_message_carbons = false;
      file_transfer_proxies = "proxy.headcounter.org";
      ft_send_local_ips = true;
      gpg_sign_presence = true;
      hostname = "aszlig.net";
      http_auth = "ask";
      ignore_ssl_errors = "";
      ignore_unknown_contacts = false;
      is_zeroconf = false;
      keep_alive_every_foo_secs = "55";
      keep_alives_enabled = true;
      keyid = "4DFD43EC834B6901BDA2BAAC1DE8E48E57DB5436";
      keyname = ''aszlig <"^[0-9]+$"@regexmail.net>'';
      last_archiving_time = "1970-01-01T00:00:00Z";
      last_status_msg = "";
      listen_to_network_manager = true;
      log_encrypted_sessions = true;
      minimized_gc = "";
      "msgwin-height" = "440";
      "msgwin-width" = "480";
      "msgwin-x-position" = "-1";
      "msgwin-y-position" = "-1";
      name = "aszlig";
      no_log_for = "";
      ping_alive_every_foo_secs = "120";
      ping_alives_enabled = true;
      priority = "5";
      proxy = "";
      publish_location = false;
      publish_tune = false;
      request_receipt = true;
      resource = "redmoon";
      restore_last_status = false;
      savepass = true;
      send_idle_time = true;
      send_os_info = true;
      send_time_info = true;
      ssl_fingerprint_sha1 = "8D:BC:E5:46:AB:B3:53:F7:36:B3:"
                           + "66:0D:B4:B7:83:32:65:BA:A8:EF";
      subscribe_activity = true;
      subscribe_location = true;
      subscribe_mood = true;
      subscribe_nick = true;
      subscribe_tune = true;
      subscription_request_msg = "";
      sync_with_global_status = true;
      test_ft_proxies_on_startup = true;
      time_for_ping_alive_answer = "60";
      try_connecting_for_foo_secs = "60";
      use_custom_host = false;
      use_env_http_proxy = false;
      use_ft_proxies = true;
      use_srv = true;
      warn_when_insecure_password = true;
      warn_when_insecure_ssl_connection = true;
      zeroconf_email = "";
      zeroconf_first_name = "";
      zeroconf_jabber_id = "";
      zeroconf_last_name = "";
    };
  };

  defaultstatusmsg = {
    away = {
      enabled = false;
      message = "Be right back.";
    };

    chat = {
      enabled = false;
      message = "I'm free for chat.";
    };

    dnd = {
      enabled = false;
      message = "Do not disturb.";
    };

    invisible = {
      enabled = false;
      message = "Bye!";
    };

    offline = {
      enabled = false;
      message = "Bye!";
    };

    online = {
      enabled = false;
      message = "I'm available.";
    };

    xa = {
      enabled = false;
      message = "I'm not available.";
    };
  };

  statusmsg = let
    defaults = {
      activity = "";
      activity_text = "";
      message = "";
      mood = "";
      mood_text = "";
      subactivity = "";
    };
    applyDefaults = const (attrs: defaults // attrs);
  in mapAttrs applyDefaults {
    zone.activity = "working";
    zone.subactivity = "coding";
    zone.message = "In The Zone[TM]";

    rofa.activity = "working";
    rofa.activity_text = "Blinded by the lights...";
    rofa.subactivity = "other";
    rofa.message = "RoFa";

    kernel.mood = "happy";
    kernel.message = "Kerneling down for reboot NOW.";

    sleep.activity = "inactive";
    sleep.subactivity = "sleeping";
    sleep.mood = "sleepy";
    sleep.message = "Sleeping the hell out of here...";

    _last_away = {};
    _last_chat = {};
    _last_dnd = {};
    _last_invisible = {};
    _last_offline = {};
    _last_online = {};
    _last_xa = {};
  };

  soundevents = {
    contact_connected = {
      enabled = false;
      path = "connected.wav";
    };

    contact_disconnected = {
      enabled = false;
      path = "disconnected.wav";
    };

    first_message_received = {
      enabled = true;
      path = "message1.wav";
    };

    gmail_received = {
      enabled = false;
      path = "message1.wav";
    };

    message_sent = {
      enabled = false;
      path = "sent.wav";
    };

    muc_message_highlight = {
      enabled = true;
      path = "gc_message1.wav";
    };

    muc_message_received = {
      enabled = false;
      path = "gc_message2.wav";
    };

    next_message_received_focused = {
      enabled = false;
      path = "message2.wav";
    };

    next_message_received_unfocused = {
      enabled = true;
      path = "message2.wav";
    };
  };

  proxies.Tor = {
    bosh_content = "text/xml; charset=utf-8";
    bosh_hold = "2";
    bosh_http_pipelining = false;
    bosh_uri = "";
    bosh_useproxy = false;
    bosh_wait = "30";
    bosh_wait_for_restart_response = false;
    host = "localhost";
    pass = "";
    port = "9050";
    type = "socks5";
    useauth = false;
    user = "";
  };

  themes = {
    blue = {
      accountbgcolor = "#0c232e";
      accountfont = "Liberation Mono 8";
      accountfontattrs = "B";
      accounttextcolor = "#ffffff";
      bannerbgcolor = "#0f4864";
      bannerfont = "Liberation Mono Bold 12";
      bannerfontattrs = "B";
      bannertextcolor = "#ffffff";
      contactbgcolor = "#0c232b";
      contactfont = "Liberation Mono Bold 8";
      contactfontattrs = "";
      contacttextcolor = "#ffffff";
      groupbgcolor = "#18515f";
      groupfont = "Liberation Mono Bold 8";
      groupfontattrs = "I";
      grouptextcolor = "#ffffff";
      state_composing_color = "green4";
      state_gone_color = "grey";
      state_inactive_color = "grey62";
      state_muc_directed_msg_color = "red2";
      state_muc_msg_color = "mediumblue";
      state_paused_color = "mediumblue";
    };

    default = {
      accountbgcolor = "";
      accountfont = "";
      accountfontattrs = "B";
      accounttextcolor = "";
      bannerbgcolor = "";
      bannerfont = "";
      bannerfontattrs = "B";
      bannertextcolor = "";
      contactbgcolor = "";
      contactfont = "";
      contactfontattrs = "";
      contacttextcolor = "";
      groupbgcolor = "";
      groupfont = "";
      groupfontattrs = "I";
      grouptextcolor = "";
      state_composing_color = "green4";
      state_gone_color = "grey";
      state_inactive_color = "grey62";
      state_muc_directed_msg_color = "red2";
      state_muc_msg_color = "mediumblue";
      state_paused_color = "mediumblue";
    };

    green = {
      accountbgcolor = "#94aa8c";
      accountfont = "";
      accountfontattrs = "B";
      accounttextcolor = "";
      bannerbgcolor = "#94aa8c";
      bannerfont = "";
      bannerfontattrs = "B";
      bannertextcolor = "";
      contactbgcolor = "";
      contactfont = "";
      contactfontattrs = "";
      contacttextcolor = "#000000";
      groupbgcolor = "#eff3e7";
      groupfont = "";
      groupfontattrs = "I";
      grouptextcolor = "#0000ff";
      state_composing_color = "green4";
      state_gone_color = "grey";
      state_inactive_color = "grey62";
      state_muc_directed_msg_color = "red2";
      state_muc_msg_color = "mediumblue";
      state_paused_color = "mediumblue";
    };

    grocery = {
      accountbgcolor = "#6bbe18";
      accountfont = "";
      accountfontattrs = "B";
      accounttextcolor = "";
      bannerbgcolor = "#108abd";
      bannerfont = "";
      bannerfontattrs = "B";
      bannertextcolor = "";
      contactbgcolor = "#efb26b";
      contactfont = "";
      contactfontattrs = "";
      contacttextcolor = "#000000";
      groupbgcolor = "#ceefad";
      groupfont = "";
      groupfontattrs = "I";
      grouptextcolor = "#12125a";
      state_composing_color = "green4";
      state_gone_color = "grey";
      state_inactive_color = "grey62";
      state_muc_directed_msg_color = "red2";
      state_muc_msg_color = "mediumblue";
      state_paused_color = "mediumblue";
    };

    human = {
      accountbgcolor = "#996442";
      accountfont = "";
      accountfontattrs = "B";
      accounttextcolor = "";
      bannerbgcolor = "#996442";
      bannerfont = "";
      bannerfontattrs = "B";
      bannertextcolor = "";
      contactbgcolor = "";
      contactfont = "";
      contactfontattrs = "";
      contacttextcolor = "#000000";
      groupbgcolor = "#e3ca94";
      groupfont = "";
      groupfontattrs = "I";
      grouptextcolor = "#ab5920";
      state_composing_color = "green4";
      state_gone_color = "grey";
      state_inactive_color = "grey62";
      state_muc_directed_msg_color = "red2";
      state_muc_msg_color = "mediumblue";
      state_paused_color = "mediumblue";
    };

    marine = {
      accountbgcolor = "#918caa";
      accountfont = "";
      accountfontattrs = "B";
      accounttextcolor = "";
      bannerbgcolor = "#918caa";
      bannerfont = "";
      bannerfontattrs = "B";
      bannertextcolor = "";
      contactbgcolor = "";
      contactfont = "";
      contactfontattrs = "";
      contacttextcolor = "#000000";
      groupbgcolor = "#e9e7f3";
      groupfont = "";
      groupfontattrs = "I";
      grouptextcolor = "";
      state_composing_color = "green4";
      state_gone_color = "grey";
      state_inactive_color = "grey62";
      state_muc_directed_msg_color = "red2";
      state_muc_msg_color = "mediumblue";
      state_paused_color = "mediumblue";
    };
  };
}
