{ jsTalkies
, cssNormalize, cssMain
, id_txt, cv_pdf
, fontsQuattrocentoLatin, fontsOpenSansLatin
, notes-html-snippet, projects-html-snippet, posts-html-snippet
}:

''
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <title>Profpatsch’s Lair</title>
    <meta name="description" content="Oh the things you’ll see …">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <!--
    prevent favicon request, based on answers in
    https://stackoverflow.com/questions/1321878/how-to-prevent-favicon-ico-requests
    TODO: create favicon
    -->
    <link rel="icon" href="data:,">

    <link rel="preload" href="${jsTalkies}" as="script">
    <link rel="preload" href="${cssNormalize}" as="style">
    <link rel="preload" href="${cssMain}" as="style">
    <!-- TODO: the font type is a magic string here -->
    <link rel="preload" href="${fontsQuattrocentoLatin}" as="font" type="font/woff2" crossorigin>
    <link rel="preload" href="${fontsOpenSansLatin}" as="font" type="font/woff2" crossorigin>

    <link rel="stylesheet" href="${cssNormalize}">
    <link rel="stylesheet" href="${cssMain}">

    <script src="${jsTalkies}"></script>
  </head>
  <body>

    <header id="title">
      <h1>Profpatsch</h1>
      <h2>
        <script>
          appendTalkies(document.querySelector('#title h2'));
        </script>
        <noscript>
          Oh the things you’ll see …
        </noscript>
      </h2>
      <hr />
    </header>

    <main>
      <p id="codeblock">
        <a href="${id_txt}">id.txt</a><br>
        <a href="${cv_pdf}">CV</a>
      </p>

      <p>Hey there.</p>

      <h1>Notes</h1>
      ${notes-html-snippet}

      <h1>Projects</h1>
      ${projects-html-snippet}

      <h1>Posts</h1>
      ${posts-html-snippet}

    </main>
  </body>
</html>
''
