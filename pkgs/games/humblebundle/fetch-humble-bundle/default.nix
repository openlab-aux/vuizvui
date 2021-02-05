{ stdenv, lib, curl, cacert, writeText, writeScript, fetchFromGitHub, fetchpatch
, python, python3, pythonPackages

# Dependencies for the captcha solver
, pkgconfig, qt5

, email, password
}:

{ name ? null
, machineName
, downloadName ? "Download"
, suffix ? "humblebundle"
, md5
}:

let
  cafile = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  getCaptcha = let
    injectedJS = ''
      function waitForResponse() {
        try {
          var response = grecaptcha.getResponse();
        } catch(_) {
          return setTimeout(waitForResponse, 50);
        }
        if (response != "")
          document.title = response;
        else
          setTimeout(waitForResponse, 50);
      }

      waitForResponse();
    '';

    escapeCString = lib.replaceStrings ["\"" "\n"] ["\\\"" "\\n"];

    application = writeText "captcha.cc" ''
      #include <QApplication>
      #include <QWebEngineView>
      #include <QTcpServer>
      #include <QQuickWebEngineProfile>

      int main(int argc, char **argv) {
        QApplication *app = new QApplication(argc, argv);
        QTcpServer *server = new QTcpServer();
        QWebEngineView *browser = new QWebEngineView();

        QQuickWebEngineProfile::defaultProfile()->setOffTheRecord(true);

        if (!server->listen(QHostAddress::LocalHost, 18123)) {
          qCritical() << "Unable to listen on port 18123!";
          return 1;
        }

        qInfo() << "Waiting for connection from the HB downloader...";
        if (!server->waitForNewConnection(-1)) {
          qCritical() << "Unable to accept the connection!";
          return 1;
        }
        qInfo() << "Connection established, spawning window to solve captcha.";

        QTcpSocket *sock = server->nextPendingConnection();

        browser->load(QUrl("https://www.humblebundle.com/user/captcha"));
        browser->show();

        browser->connect(browser, &QWebEngineView::loadFinished, [=]() {
          browser->page()->runJavaScript("${escapeCString injectedJS}");
          browser->connect(
            browser, &QWebEngineView::titleChanged, [=](const QString &title) {
              sock->write(title.toUtf8());
              sock->flush();
              sock->waitForBytesWritten();
              sock->close();
              server->close();
              app->quit();
            }
          );
        });

        return app->exec();
      }
    '';

  in stdenv.mkDerivation {
    name = "get-captcha";

    dontUnpack = true;

    nativeBuildInputs = [ pkgconfig (qt5.wrapQtAppsHook or null) ];
    buildInputs = [ qt5.qtbase qt5.qtwebengine ];
    preferLocalBuild = true;

    buildPhase = ''
      g++ $(pkg-config --libs --cflags Qt5WebEngineWidgets Qt5WebEngine) \
        -Wall -std=c++11 -o get-captcha ${application}
    '';

    installPhase = ''
      install -vD get-captcha "$out/bin/get-captcha"
    '';
  };

  getGuard = writeScript "get-guard" ''
    #!${python3.interpreter}
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
      sock.bind(('localhost', 18129))
      sock.listen(1)
      with sock.accept()[0] as conn:
        guard = input("Guard code: ")
        conn.sendall(guard.encode())
  '';

  humbleAPI = pythonPackages.buildPythonPackage rec {
    name = "humblebundle-${version}";
    version = "0.1.1";

    src = fetchFromGitHub {
      owner = "saik0";
      repo = "humblebundle-python";
      rev = version;
      sha256 = "1kcg42nh7sbjabim1pbqx14468pypznjy7fx2bv7dicy0sqd9b8j";
    };

    patches = [ ./guard-code.patch ];

    postPatch = ''
      sed -i -e '/^LOGIN_URL *=/s,/login,/processlogin,' humblebundle/client.py
      sed -i -e '/self\.supports_canonical.*data.*supports_canonical/d' \
        humblebundle/models.py
    '';

    propagatedBuildInputs = [ pythonPackages.requests ];
  };

  pyStr = str: "'${lib.escape ["'" "\\"] str}'";

  getDownloadURL = writeText "gethburl.py" ''
    import socket, sys, time, humblebundle

    def get_products(client):
      gamekeys = client.get_gamekeys()
      for gamekey in gamekeys:
        order = hb.get_order(gamekey)
        if order.subproducts is None:
          continue
        for subproduct in order.subproducts:
          prodname = subproduct.human_name.encode('ascii', 'replace')
          downloads = [(download.machine_name, download.download_struct)
                       for download in subproduct.downloads]
          yield ((subproduct.machine_name, prodname), downloads)

    def find_download(downloads):
      for machine_name, dstruct in sum(downloads.values(), []):
        if machine_name == ${pyStr machineName}:
          for ds in dstruct:
            if ds.name == ${pyStr downloadName}:
              return ds
          print >>sys.stderr, \
            ${pyStr "Unable to find ${downloadName} for ${machineName}!"}
          print >>sys.stderr, 'Available download types:'
          for ds in dstruct:
            print >>sys.stderr, "  " + ds.name
          raise SystemExit(1)

    def login_with_captcha(hb):
      print >>sys.stderr, "Solving a captcha is required to log in."
      print >>sys.stderr, "Please run " ${pyStr (toString getCaptcha)} \
                          "/bin/get-captcha now."
      sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
      print >>sys.stderr, "Waiting for connection",
      i = 0
      while sock.connect_ex(("127.0.0.1", 18123)) != 0:
        time.sleep(0.1)
        if i % 10 == 0:
          sys.stderr.write('.')
          sys.stderr.flush()
        i += 1
      print >>sys.stderr, " connected."
      print >>sys.stderr, "Waiting for captcha to be solved..."
      response = sock.recv(4096)
      sock.close()
      print >>sys.stderr, "Captcha solved correctly, logging in."
      api_login(hb, recaptcha_response=response)

    def login_with_guard(hb, skip_code):
      print >>sys.stderr, "A guard code has been sent to your email address."
      print >>sys.stderr, "Please run " ${pyStr (toString getGuard)} " now."
      sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
      print >>sys.stderr, "Waiting for connection",
      # XXX: DRY!
      i = 0
      while sock.connect_ex(("127.0.0.1", 18129)) != 0:
        time.sleep(0.1)
        if i % 10 == 0:
          sys.stderr.write('.')
          sys.stderr.flush()
        i += 1
      print >>sys.stderr, " connected."
      print >>sys.stderr, "Waiting for guard code..."
      response = sock.recv(4096)
      sock.close()
      print >>sys.stderr, "Guard code supplied, logging in."
      api_login(hb, captcha_skip_code=skip_code, guard_code=response)

    def api_login(hb, recaptcha_response=None,
                  captcha_skip_code=None, guard_code=None):
      try:
        hb.login(${pyStr email}, ${pyStr password},
                 recaptcha_response=recaptcha_response,
                 captcha_skip_code=captcha_skip_code, guard_code=guard_code)
      except humblebundle.exceptions.HumbleCaptchaException:
        login_with_captcha(hb)
      except humblebundle.exceptions.HumbleGuardRequiredException as e:
        login_with_guard(hb, e.captcha_skip_code)

    hb = humblebundle.HumbleApi()
    api_login(hb)

    products = dict(get_products(hb))
    dstruct = find_download(products)

    if dstruct is None:
      print >>sys.stderr, ${pyStr "Cannot find download for ${machineName}!"}
      print >>sys.stderr, 'Available machine names:'
      for name, dstructs in sorted(products.items(), key=lambda x: x[0]):
        print >>sys.stderr, "  * " + name[1]
        print >>sys.stderr, "    " + ', '.join(map(lambda x: x[0], dstructs))
      raise SystemExit(1)
    elif dstruct.md5 != ${pyStr md5}:
      print >>sys.stderr, \
        ${pyStr "MD5 for ${machineName} is not ${md5} but "} \
        + dstruct.md5 + '.'
      raise SystemExit(1)
    else:
      print dstruct.url.web
  '';
in stdenv.mkDerivation {
  name = if name != null then name else "${machineName}.${suffix}";
  outputHashAlgo = "md5";
  outputHash = md5;

  preferLocalBuild = true;
  buildInputs = [ python humbleAPI ];

  buildCommand = ''
    url="$(python "${getDownloadURL}")"
    header "downloading $name from $url"
    "${curl.bin or curl}/bin/curl" --cacert "${cafile}" --fail \
      --output "$out" "$url"
    stopNest
  '';
}
