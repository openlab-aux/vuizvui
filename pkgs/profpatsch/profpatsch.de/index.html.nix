{ jsTalkies
, cssNormalize, cssMain
, id_txt, cv_pdf
, fontsQuattrocentoLatin, fontsOpenSansLatin
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

      <p>Hey there. Seems like you landed on my webpage.</p>

      <p>Well, more like a simple HTML page. But that’s fine for the moment, since I don’t use it as a web presence yet.
        I may when I finally write those articles I have in mind, but for now you are stuck with this.</p>

      <p>That’s not a problem, though, since other people do a lot better job at designing these. For example the folks at Youtube, StackExchange or Github.</p>

      <p>If you are searching for a skilled programmer to do some work for you, <a href="${cv_pdf}">look no further</a>.
        In case you want to know what I do in my free time, you can <a href="https://twitter.com/Profpatsch/">follow me on Twitter</a>.</p>
      <p>On another note, you can find me practically everywhere under my nick (Github, G+, several mailinglists, SO, &amp;c.)

      <p>There you go. Cya!</p>


    </main>
  </body>
</html>
''
