{ stdenv, curl, cacert, writeText, fetchFromGitHub, fetchpatch
, python, pythonPackages

# Dependencies for the captcha solver
, pkgconfig, qt5, runCommandCC

, email, password
}:

{ name ? null, machineName, downloadName ? "Download", suffix ? "humblebundle", md5 }: let
  cafile = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  getCaptcha = let
    injectedJS = ''
      function waitForResponse() {
        var response = captcha.get_response();
        if (response != "")
          document.title = response;
        else
          setTimeout(waitForResponse, 50);
      }

      waitForResponse();
    '';

    escapeCString = stdenv.lib.replaceStrings ["\"" "\n"] ["\\\"" "\\n"];

    application = writeText "captcha.cc" ''
      #include <QApplication>
      #include <QWebEngineView>
      #include <QTcpServer>

      int main(int argc, char **argv) {
        QApplication *app = new QApplication(argc, argv);
        QTcpServer *server = new QTcpServer();
        QWebEngineView *browser = new QWebEngineView();

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

  in runCommandCC "get-captcha" {
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ qt5.qtbase qt5.qtwebengine ];
  } ''
    g++ $(pkg-config --libs --cflags Qt5WebEngineWidgets) \
      -Wall -std=c++11 -o "$out" ${application}
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

    postPatch = ''
      sed -i -e '/^LOGIN_URL *=/s,/login,/processlogin,' humblebundle/client.py
    '';

    propagatedBuildInputs = [ pythonPackages.requests ];
  };

  pyStr = str: "'${stdenv.lib.escape ["'" "\\"] str}'";

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
          yield (prodname, downloads)

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
      print >>sys.stderr, "Please run " ${pyStr (toString getCaptcha)} " now."
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
      hb.login(${pyStr email}, ${pyStr password}, recaptcha_response=response)

    hb = humblebundle.HumbleApi()
    try:
      hb.login(${pyStr email}, ${pyStr password})
    except humblebundle.exceptions.HumbleCaptchaException:
      login_with_captcha(hb)

    products = dict(get_products(hb))
    dstruct = find_download(products)

    if dstruct is None:
      print >>sys.stderr, ${pyStr "Cannot find download for ${machineName}!"}
      print >>sys.stderr, 'Available machine names:'
      for name, dstructs in sorted(products.items(), key=lambda x: x[0]):
        print >>sys.stderr, "  * " + name
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

  buildInputs = [ python humbleAPI ];

  buildCommand = ''
    url="$(python "${getDownloadURL}")"
    header "downloading $name from $url"
    "${curl.bin or curl}/bin/curl" --cacert "${cafile}" --fail \
      --output "$out" "$url"
    stopNest
  '';
}
