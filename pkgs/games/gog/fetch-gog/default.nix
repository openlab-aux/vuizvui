{ stdenv, lib, curl, writeText, runCommandCC, python3Packages, cacert
, pkgconfig, qt5

, email, password
}:

{ productId, downloadName, sha256, downloadType ? "installer", suffix ? "sh"
, name ? "${toString productId}-${downloadName}-${downloadType}.${suffix}"
}:

let
  getCaptcha = let
    mkCString = val: let
      escaped = lib.replaceStrings ["\"" "\\" "\n"] ["\\\"" "\\\\" "\\n"] val;
    in "\"${escaped}\"";

    injectedJS = ''
      var user_input = document.getElementById('login_username');
      var pass_input = document.getElementById('login_password');
      var submit_button = document.getElementById('login_login');

      user_input.value = ${builtins.toJSON email};
      user_input.style.display = 'none';
      pass_input.value = ${builtins.toJSON password};
      pass_input.style.display = 'none';

      submit_button.style.display = 'none';

      function waitForResponse() {
        var response = grecaptcha.getResponse();
        if (response != "")
          submit_button.click();
        else
          setTimeout(waitForResponse, 50);
      }

      waitForResponse();
    '';

    application = writeText "captcha.cc" ''
      #include <QApplication>
      #include <QWebEngineView>
      #include <QTcpServer>
      #include <QQuickWebEngineProfile>
      #include <QUrlQuery>

      // Taken from lgogdownloader (https://github.com/Sude-/lgogdownloader):
      static QString clientId = "46899977096215655";
      static QString clientSecret =
        "9d85c43b1482497dbbce61f6e4aa173a433796eeae2ca8c5f6129f2dc4de46d9";
      static QString redirectUri =
        "https://embed.gog.com/on_login_success?origin=client";

      static QUrl getAuthUrl() {
        QUrl url("https://auth.gog.com/auth");

        QUrlQuery query;
        query.addQueryItem("client_id", clientId);
        query.addQueryItem("redirect_uri", redirectUri);
        query.addQueryItem("response_type", "code");
        query.addQueryItem("layout", "default");
        query.addQueryItem("brand", "gog");

        url.setQuery(query);
        return url;
      }

      static QUrl getTokenUrl(const QString &code) {
        QUrl url("https://auth.gog.com/token");

        QUrlQuery query;
        query.addQueryItem("client_id", clientId);
        query.addQueryItem("client_secret", clientSecret);
        query.addQueryItem("grant_type", "authorization_code");
        query.addQueryItem("code", code);
        query.addQueryItem("redirect_uri", redirectUri);

        url.setQuery(query);
        return url;
      }

      int main(int argc, char **argv) {
        QApplication *app = new QApplication(argc, argv);
        QTcpServer *server = new QTcpServer();
        QWebEngineView *browser = new QWebEngineView();

        QQuickWebEngineProfile::defaultProfile()->setOffTheRecord(true);

        if (!server->listen(QHostAddress::LocalHost, 18321)) {
          qCritical() << "Unable to listen on port 18321!";
          return 1;
        }

        qInfo() << "Waiting for connection from the GOG login helper...";
        if (!server->waitForNewConnection(-1)) {
          qCritical() << "Unable to accept the connection!";
          return 1;
        }
        qInfo() << "Connection established, spawning window to login.";

        QTcpSocket *sock = server->nextPendingConnection();

        browser->load(getAuthUrl());
        browser->show();

        browser->connect(browser, &QWebEngineView::loadFinished, [=]() {
          browser->page()->runJavaScript(${mkCString injectedJS});
          browser->connect(
            browser, &QWebEngineView::urlChanged, [=](const QUrl &newurl
          ) {
            QUrlQuery newquery(newurl.query());
            QString code = newquery.queryItemValue("code");
            sock->write(getTokenUrl(code).toEncoded());
            sock->flush();
            sock->waitForBytesWritten();
            sock->close();
            server->close();
            app->quit();
          });
        });

        return app->exec();
      }
    '';

  in runCommandCC "get-captcha" {
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ qt5.qtbase qt5.qtwebengine ];
  } ''
    g++ $(pkg-config --libs --cflags Qt5WebEngineWidgets Qt5WebEngine) \
      -Wall -std=c++11 -o "$out" ${application}
  '';

  mkPyStr = str: "'${stdenv.lib.escape ["'" "\\"] (toString str)}'";

  fetcher = writeText "fetch-gog.py" ''
    import sys, socket, time
    from urllib.request import urlopen, Request
    from urllib.error import HTTPError
    from json import loads

    from tabulate import tabulate

    class GogFetcher:
      def __init__(self, product_id, download_type, download_name):
        sys.stderr.write("Solving a captcha is required to log in.\n")
        sys.stderr.write("Please run " ${mkPyStr getCaptcha} " now.\n")
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sys.stderr.write("Waiting for connection")
        i = 0
        while sock.connect_ex(("127.0.0.1", 18321)) != 0:
          time.sleep(0.1)
          if i % 10 == 0:
            sys.stderr.write('.')
            sys.stderr.flush()
          i += 1
        sys.stderr.write(" connected.\n")
        sys.stderr.write("Waiting for captcha to be solved...\n")
        token_url = sock.recv(4096)
        sock.close()
        sys.stderr.write("Captcha solved correctly, logging in.\n")
        response = urlopen(
          token_url.decode(),
          cafile=${mkPyStr "${cacert}/etc/ssl/certs/ca-bundle.crt"}
        )

        self.product_id = product_id
        self.download_type = download_type
        self.download_name = download_name
        self.access_token = loads(response.read())['access_token']

      def request(self, url):
        headers = {"Authorization": "Bearer " + self.access_token}
        return loads(urlopen(
          Request(url, headers=headers),
          cafile=${mkPyStr "${cacert}/etc/ssl/certs/ca-bundle.crt"}
        ).read())

      def strip_dtype(self, dtype):
        if dtype.endswith('es'):
          return dtype[:-2]
        elif dtype.endswith('s'):
          return dtype[:-1]
        else:
          return dtype

      def list_downloads(self):
        url = "https://api.gog.com/products/" + self.product_id \
            + "?expand=downloads"
        downloads = self.request(url)['downloads']
        table = []
        for dtype, dloads in downloads.items():
          for dload in dloads:
            for dlfile in dload['files']:
              table.append([
                self.strip_dtype(dtype), dload.get('os', ""),
                dload.get('language_full', ""),
                dlfile['id'], dlfile['size'], dlfile['downlink']
              ])
        sys.stderr.write(tabulate(table, headers=[
          'Download type',
          'Operating system',
          'Language',
          'Identifier',
          'Size',
          'URL'
        ]) + "\n")

      def fetch(self):
        url = "http://api.gog.com/products/" + self.product_id \
            + "/downlink/" + self.download_type + "/" + self.download_name
        try:
          download = self.request(url)
        except HTTPError:
          m = "Download {!r} with type {!r} not found.\nValid downloads are:\n"
          sys.stderr.write(m.format(self.download_name, self.download_type))
          self.list_downloads()
          raise SystemExit(1)
        else:
          print(download['downlink'])

    GogFetcher(sys.argv[1], sys.argv[2], sys.argv[3]).fetch()
  '';

in stdenv.mkDerivation {
  inherit name;
  outputHashAlgo = "sha256";
  outputHash = sha256;

  nativeBuildInputs = [ curl python3Packages.tabulate ];

  buildCommand = ''
    url="$(${python3Packages.python.interpreter} ${fetcher} \
      ${toString productId} \
      ${lib.escapeShellArg downloadType} \
      ${lib.escapeShellArg downloadName})"
    header "downloading $name from $url"
    curl \
      --cacert ${lib.escapeShellArg "${cacert}/etc/ssl/certs/ca-bundle.crt"} \
      --fail --output "$out" "$url"
    stopNest
  '';
}
