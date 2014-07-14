{ stdenv, fetchurl, fetchhg, python, intltool, pkgconfig, libX11, gtk
, host, pyopenssl, pythonDBus, pythonPackages, nbxmpp

, enableJingle ? true, farstream ? null, gst_plugins_bad ? null
,                      libnice ? null
, enableE2E ? true
, enableRST ? true
, enableSpelling ? true, gtkspell ? null
, enableNotifications ? false
, enableLaTeX ? false, texLive ? null

# FIXME: Remove after final release.
, autoconf, automake, libtool
}:

assert enableJingle -> farstream != null && gst_plugins_bad != null
                    && libnice != null;
assert enableE2E -> pythonPackages.pycrypto != null;
assert enableRST -> pythonPackages.docutils != null;
assert enableSpelling -> gtkspell != null;
assert enableNotifications -> pythonPackages.notify != null;
assert enableLaTeX -> texLive != null;

with stdenv.lib;

stdenv.mkDerivation rec {
  name = "gajim-${version}";
  version = "0.16";

  src = fetchhg {
    url = "http://hg.gajim.org/gajim";
    rev = "f38e5fd4f8e3";
    sha256 = "17wmy355znfkv4fbwa4hzicr17k8if75mh3b14yphszfw3bh3mfw";
  };

  preConfigure = ''
    sed -e 's/\<which\>/type -P/' autogen.sh | sh
  '';

  postPatch = ''
    sed -i -e '0,/^[^#]/ {
      /^[^#]/i export \\\
        PYTHONPATH="'"$PYTHONPATH\''${PYTHONPATH:+:}\$PYTHONPATH"'" \\\
        GST_PLUGIN_PATH="'"\$GST_PLUGIN_PATH''${GST_PLUGIN_PATH:+:}${""
        }$GST_PLUGIN_PATH"'"
    }' scripts/gajim.in

    sed -i -e 's/return helpers.is_in_path('"'"'nslookup.*/return True/' \
      src/features_window.py
    sed -i -e '/is_in_path\|return \[.host/'"s|'host'|'${host}/bin/host'|" \
      src/common/resolver.py
  '' + optionalString enableSpelling ''
    sed -i -e 's|=.*find_lib.*|= "${gtkspell}/lib/libgtkspell.so"|'   \
      src/gtkspell.py
  '' + optionalString enableLaTeX ''
    sed -i -e "s|try_run(.'dvipng'|try_run(['${texLive}/bin/dvipng'|" \
           -e "s|try_run(.'latex'|try_run(['${texLive}/bin/latex'|"   \
           -e 's/tmpfd.close()/os.close(tmpfd)/'                      \
           src/common/latex.py
  '';

  buildInputs = [
    python intltool pkgconfig libX11
    pythonPackages.pygobject pythonPackages.pyGtkGlade
    pythonPackages.sqlite3 pythonPackages.pyasn1
    pythonPackages.pyxdg nbxmpp
    pyopenssl pythonDBus
    # FIXME: Remove after final release.
    autoconf automake libtool
  ] ++ optionals enableJingle [ farstream gst_plugins_bad libnice ]
    ++ optional enableE2E pythonPackages.pycrypto
    ++ optional enableRST pythonPackages.docutils
    ++ optional enableNotifications pythonPackages.notify
    ++ optional enableLaTeX texLive;

  postInstall = ''
    install -m 644 -t "$out/share/gajim/icons/hicolor" \
                      "icons/hicolor/index.theme"
  '';

  enableParallelBuilding = true;

  meta = {
    homepage = "http://gajim.org/";
    description = "Jabber client written in PyGTK";
    license = licenses.gpl3Plus;
    maintainers = [ maintainers.raskin maintainers.aszlig ];
  };
}
