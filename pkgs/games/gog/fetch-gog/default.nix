{ stdenv, lib, curl, writeText, runCommandCC, python3Packages, cacert
, pkgconfig, qt5

, email, password
}:

{ productId, downloadName, sha256, downloadType ? "installer", suffix ? "sh"
, name ? "${toString productId}-${downloadName}-${downloadType}.${suffix}"
}:

let
  # Taken from lgogdownloader (https://github.com/Sude-/lgogdownloader):
  clientId = "46899977096215655";
  clientSecret = "9d85c43b1482497dbbce61f6e4aa173a"
               + "433796eeae2ca8c5f6129f2dc4de46d9";
  redirectUri = "https://embed.gog.com/on_login_success?origin=client";

  urlencode = url: query: let
    urlquote = isQstring: val: let
      extraSafeChars = lib.optionalString (!isQstring) "/:";
      safeChars = lib.lowerChars ++ lib.upperChars
               ++ lib.stringToCharacters ("0123456789_.-" + extraSafeChars);
      charList = lib.stringToCharacters val;
      hexify = chr: "%${import ./hexify-char.nix chr}";
      quoteChar = chr: if lib.elem chr safeChars then chr else hexify chr;
    in lib.concatMapStrings quoteChar charList;
    mkKeyVal = key: val: "${urlquote true key}=${urlquote true val}";
    qstring = lib.concatStringsSep "&" (lib.mapAttrsToList mkKeyVal query);
  in urlquote false url + lib.optionalString (query != {}) "?${qstring}";

  authURL = urlencode "https://auth.gog.com/auth" {
    client_id = clientId;
    redirect_uri = redirectUri;
    response_type = "code";
    layout = "default";
    brand = "gog";
  };

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

      static QString clientId = ${mkCString clientId};
      static QString clientSecret = ${mkCString clientSecret};
      static QString redirectUri = ${mkCString redirectUri};

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

  mkPyStr = str: "'${stdenv.lib.escape ["'" "\\"] (toString str)}'";

  fetcher = writeText "fetch-gog.py" ''
    import sys, socket, time
    from urllib.request import urlopen, Request
    from urllib.parse import urlsplit, urlencode, parse_qs
    from urllib.error import HTTPError
    from json import loads

    import mechanicalsoup
    from tabulate import tabulate

    class GogFetcher:
      def __init__(self, product_id, download_type, download_name):
        self.product_id = product_id
        self.download_type = download_type
        self.download_name = download_name
        self.login()

      def login(self):
        browser = mechanicalsoup.StatefulBrowser()
        response = browser.open(${mkPyStr authURL})
        if "https://www.recaptcha.net/recaptcha" in response.text:
          token_url = self.login_with_captcha()
        else:
          browser.select_form('form[name="login"]')
          browser['login[username]'] = ${mkPyStr email}
          browser['login[password]'] = ${mkPyStr password}
          browser.submit_selected()

          query = parse_qs(urlsplit(browser.get_url()).query)

          if 'code' not in query:
            sys.stderr.write(
              "Unable to login with the provided GOG credentials.\n"
            )
            raise SystemExit(1)

          token_url = "https://auth.gog.com/token?" + urlencode({
            'client_id': ${mkPyStr clientId},
            'client_secret': ${mkPyStr clientSecret},
            'grant_type': 'authorization_code',
            'code': query['code'],
            'redirect_uri': ${mkPyStr redirectUri}
          })

        response = urlopen(
          token_url, cafile=${mkPyStr "${cacert}/etc/ssl/certs/ca-bundle.crt"}
        )

        self.access_token = loads(response.read())['access_token']

      def login_with_captcha(self):
        sys.stderr.write("Solving a captcha is required to log in.\n")
        sys.stderr.write("Please run " ${mkPyStr getCaptcha}
                         "/bin/get-captcha now.\n")
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
        return token_url.decode()

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
            + "?expand=downloads,expanded_dlcs,related_products"
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

  preferLocalBuild = true;

  nativeBuildInputs = [
    curl python3Packages.tabulate python3Packages.MechanicalSoup
  ];

  buildCommand = ''
    url="$(${python3Packages.python.interpreter} ${fetcher} \
      ${toString productId} \
      ${lib.escapeShellArg downloadType} \
      ${lib.escapeShellArg downloadName})"
    header "downloading $name from $url"
    curl \
      --location \
      --cacert ${lib.escapeShellArg "${cacert}/etc/ssl/certs/ca-bundle.crt"} \
      --fail --output "$out" "$url"
    stopNest
  '';
}
