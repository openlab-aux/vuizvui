{ lib, writers, gopass, writeScript, mutt

, realName ? "aszlig"
, emailAddr ? "aszlig@nix.build"
, gopassEntry ? "misc/mailfresser"
}:

let
  askYes = { __askYes = true; };
  askNo  = { __askNo = true; };
  execCommand = cmd: { __execCmd = cmd; };

  passwd = execCommand "${gopass}/bin/gopass show -o ${gopassEntry}";
  userRaw = "`${gopass}/bin/gopass show ${gopassEntry} username`";
  serverRaw = "`${gopass}/bin/gopass show ${gopassEntry} server`";

  config = {
    folder.__raw = "\"imap://${serverRaw}/\"";
    imap_user.__raw = "\"${userRaw}\"";
    imap_pass = passwd;
    smtp_url.__raw = "\"smtp://${userRaw}@${serverRaw}/\"";
    smtp_pass = passwd;
    spoolfile = "+INBOX";

    imap_idle = true;
    imap_delim_chars = "/";
    record = "+sent-mail";
    postponed = "+Drafts";

    send_multipart_alternative = askYes;
    send_multipart_alternative_filter = writers.writePython3 "multipart" {
      flakeIgnore = [ "E111" "E121" ];
    } ''
      import string
      import sys

      DELAY_SEC = 20

      in_escape = False

      original = ""
      for c in sys.stdin.read():
        if c == "'":
          original += r"\'"
        elif c == "\\":
          original += r"\\"
        elif c in "\n<>" or (in_escape and c in string.hexdigits):
          original += r'\{:X}'.format(ord(c))
          in_escape = True
          continue
        else:
          original += c
        in_escape = False

      style = '''.join([
        '#liar::after{content:"... okay, I lied. Here\'s the text:"}',
        "#original::after{content:'" + original + "'}",
        '#content{',
        f'-webkit-animation:dontbesocruelwk 0s {DELAY_SEC}s forwards;',
        f'-moz-animation:dontbesocruelmoz 0s {DELAY_SEC}s forwards;',
        f'-o-animation:dontbesocruelo 0s {DELAY_SEC}s forwards;',
        f'animation:dontbesocruel 0s {DELAY_SEC}s forwards',
        '}',
        '@-webkit-keyframes dontbesocruelwk{to{visibility:visible}}',
        '@-moz-keyframes dontbesocruelmoz{to{visibility:visible}}',
        '@-o-keyframes dontbesocruelo{to{visibility:visible}}',
        '@keyframes dontbesocruel{to{visibility:visible}}',
      ])

      document = '''.join([
        '<!DOCTYPE html><html lang="en">',
        '<head>',
        '<title>&#128169;</title>',
        f'<style>{style}</style>',
        '</head>',
        '<body>',
        '<p>',
        'This email can only be read in ',
        '<a href="https://useplaintext.email/">plain text</a>.',
        '</p>',
        '<div id="content" style="visibility:hidden">',
        '<p id="liar"></p>',
        '<pre id="original"></pre>',
        '</div>',
        '</body>',
        '</html>',
      ])

      sys.stdout.write("text/html\n\n" + document + "\n")
    '';

    use_from = true;
    from = emailAddr;
    realname = realName;
    envelope_from = true;

    move = false;
    copy = true;
    mark_old = false;
    bounce_delivered = false;

    pgp_autosign = true;
    pgp_use_gpg_agent = true;
    crypt_use_gpgme = true;
    crypt_verify_sig = true;

    history = 200;
    pager_stop = true;
    use_domain = true;
    sort.__raw = "threads";
    sort_aux.__raw = "last-date-received";
    reply_regexp = "^(re|aw)(\\[[0-9]+\\])*:[ \t]*";

    index_format = "%4C %Z %{%b %d %Y %H:%M} %-15.15L (%?l?%4l&%4c?) %s";
  };

  mkCfgValue = val:
    if val == true then "yes"
    else if val == false then "no"
    else if val == askYes then "ask-yes"
    else if val == askNo then "ask-no"
    else if val ? __execCmd then "\"`${val.__execCmd}`\""
    else if val ? __raw then val.__raw
    else if lib.isInt val then toString val
    else lib.escapeShellArg val;

  genConfig = cfg: let
    mkKeyVal = key: val: "set ${key}=${mkCfgValue val}";
  in lib.concatStringsSep "\n" (lib.mapAttrsToList mkKeyVal cfg) + "\n";

  # XXX: Uuuugly, clean up ASAP.
  trailer = ''
    mailboxes +INBOX +catapult +list-errors +list-staff +root@mf.de

    ignore *
    unignore date from to cc subject x-mailer resent-from reply-to user-agent
    hdr_order date from to cc subject

    macro compose "<Esc>1" "<edit-from><kill-line>aszlig <\"\^[0-9]+$\"@regexmail.net><enter>"

    # paint different quote levels
    color	quoted  	green	default
    color	quoted1 	cyan	default
    color	quoted2 	yellow	default
    color	quoted3		red	default

    color normal    white black
    color attachment brightyellow black
    color hdrdefault cyan black
    color indicator black cyan
    color markers   brightred black
    color quoted    green black
    color signature cyan black
    color status    brightgreen blue
    color tilde     blue black
    color tree      red black

    color body brightred default "[\-\.+_a-zA-Z0-9]+@[\-\.a-zA-Z0-9]+" # email
    color body brightblue default "(http|ftp)://[\-\.\,/+=&%~_:?\#a-zA-Z0-9]+" # URL
    color body brightgreen default "(^| |[-.[:alnum:]]+:+)~?\(/[-_.'[:alnum:]]+\)+/?" # Unix file path
    color body brightgreen	default "(^| +)[[:alpha:]]:[-_.[:alnum:]\]+" # DOS file path
    color body brightmagenta default "(^|[ '\"]+)\\$[[:alpha:]][-_[:alpha:]]+" # variable
    color body brightred default "(^| )[*_]+[-&[:alnum:]]+[*_]( |$)" # bold/underline
    color body yellow default "(^| )[;:8][-^o]?[)>(|/\\DP]+" # smiley
    color body red default "[!?]{3,}" # exclamation
    color body green default "^ *[-+*o] +" # list item

    # date formats
    color body cyan default "[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?( ?(AM|PM|am|pm))?( +[+-][0-9]{4})?"
    color body cyan default "(\(19|20\)?[0-9]{2}/[01]?[0-9]/[0123]?[0-9]|[0123]?[0-9][/.][01]?[0-9][/.]\(19|20\)?[0-9]{2})(( at)? +[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?( ?(AM|PM|am|pm))?( +[+-][0-9]{4})?)?"
    color body cyan default "((Sun(day)?|Mon(day)?|Tue(sday)?|Wed(nesday)?|Thur(sday)?|Fri(day)?|Sat(urday)?),? +)?(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|June?|July?|Aug(ust)?|Sep(ember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)[ .]+[0-9]{1,2}(st|nd|rd|th)?,?( +(19|20)[0-9]{2}(,?( at)? [0-9]{1,2}:[0-9]{2}(:[0-9]{2})?( ?(AM|PM|am|pm))?( +[+-][0-9]{4})?)?)?"
    color body cyan default "((Sun(day)?|Mon(day)?|Tue(sday)?|Wed(nesday)?|Thur(sday)?|Fri(day)?|Sat(urday)?),? +)?[0-9]{1,2}(st|nd|rd|th)?[ .]+(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|June?|July?|Aug(ust)?|Sep(ember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?),?( +(19|20)?[0-9]{2})?(( at)? [0-9]{1,2}:[0-9]{2}(:[0-9]{2})?( ?(AM|PM|am|pm))?( +[+-][0-9]{4})?)?"

    color header   brightgreen default ^From:
    color header   brightcyan default ^To:
    color header   brightcyan default ^Reply-To:
    color header   brightcyan default ^Cc:
    color header   brightblue default ^Subject:

    color	header	brightcyan	default "^newsgroups: "
    color   header  yellow  default "^Delivered-To: "
    color   header  green   default "^sender: "
    color   header  green   default "^organi[sz]ation: "
    color   header  green   default "x-editor: "
    color   header  green   default "(x-mailer|user-agent): "
    color   header  green   default "X-Operating-System: "
    color   header  green   default "X-MimeOLE: "
    color   header  green   default "X-Accept-Language: "
    color	header	red	default	"^message-id:"
    color	header	red	default	"^in-reply-to: "
    color	header	red	default	"^references: "
    color	header	cyan	default	"^followup-to: "
    color	header	cyan	default	"^mail-followup-to: "
    color	header	cyan	default	"^reply-to: "
    color	header	magenta	default	"MIME-Version: "
    color	header	magenta	default	"Content-Type: "
    color	header	magenta	default	"Content-Transfer-Encoding: "
    color	header	magenta	default	"Content-Disposition: "
    color	header	magenta	default	"Content-Encoding: "
    color	header	magenta	default	"Content-Language: "

    color index brightblue default '~P' # from me
    color index cyan default '~p' # personal
    color index brightcyan default '~G' # PGP
    color index brightred default '~F' # flagged
    color index green default '~Q' # replied
    color index brightwhite default '~N' # new
    color index red default '~D' # deleted

    # spam
    color index magenta default "\(Resent-\)?Message-Id: <>"
    color index magenta default "\(Resent-\)?Message-Id: '<.* .*>'"
    color index magenta default "\(Resent-\)?Message-Id: localhost"
    color index magenta default "! \(Resent-\)?Message-Id: @"
    color index magenta default '~C Undisclosed.*Recipients'
  '';

in mutt.overrideAttrs (drv: {
  postInstall = (drv.postInstall or "") + ''
    echo -n ${lib.escapeShellArg (genConfig config)} > "$out/etc/Muttrc"
    echo -n ${lib.escapeShellArg trailer} >> "$out/etc/Muttrc"
  '';
})
