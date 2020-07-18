{ jsJquery
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

    <link rel="preload" href="${jsJquery}" as="script">
    <link rel="preload" href="${cssNormalize}" as="style">
    <link rel="preload" href="${cssMain}" as="style">
    <!-- TODO: the font type is a magic string here -->
    <link rel="preload" href="${fontsQuattrocentoLatin}" as="font" type="font/woff2" crossorigin>
    <link rel="preload" href="${fontsOpenSansLatin}" as="font" type="font/woff2" crossorigin>

    <link rel="stylesheet" href="${cssNormalize}">
    <link rel="stylesheet" href="${cssMain}">

    <!-- TODO: get rid of jquery -->
    <script src="${jsJquery}"></script>
  </head>
  <body>

    <header id="title">
      <h1>Profpatsch</h1>
      <h2>
        <script>
         talkies = [
             { talk: "Oh the things you’ll see …" },
             { talk: "I’ll manage." },
             { talk: "Spurdo Spärde sprölölö",
               hover: "bydon xDDDD" },
             { talk: "Where is Jessica Hyde?" },
             { talk: "Wie talkst du eigentlich?",
               hover: "Du Splasher" },
             { talk: "See you Space Cowboy",
               hover: "bang",
               link: "https://www.youtube.com/watch?v=juxjTAqB9Fs" },
             { talk: "STACKENBLOCKEN",
               hover: "21:45",
               link: "https://www.youtube.com/watch?v=d6wd78jMHDk" },
             { talk: "Don’t lose your waaaaaaay",
               hover: "I haven’t actually seen KlK …" },
             { talk: "STOP RIGHT THERE CRIMINAL SCUM",
               hover: "Let me tell you a story about my past …" },
             { talk: "Ich zähl bis zehn und mach die Augen wieder auf",
               hover: "Rivo Drei – Von Vorne" },
             { talk: "It’s so ironic when your makeup runs",
               hover: "blackest_eyes& trains& sound_of_muzak& prodigal" },
             { talk: "It’s time now, my time now. Give me my, give me my wings",
               hover: "Should you see your Maker’s face tonight // Look ’em in the eye, look ’em in the eye // And tell ’em // Never lived a lie, never took a life // But surely saved one // Hallelluja // It’s time for you to bring me home",
               link: "https://www.youtube.com/watch?v=7Ajx-ABtbVM"
             },
             { talk: "{-# LANGUAGE ImplicitProtolude #-}" },
             { talk: "overfull hbox" }
         ];
         var talkie = talkies[Math.floor(Math.random()*talkies.length)];
         var elem = null;
         var title = "";
         if (talkie.hasOwnProperty("hover")) { title = 'title="'+talkie.hover+'"'; }
         if (talkie.hasOwnProperty("link")) {
             elem = function(content, attrs) {
                 return '<a href="'+talkie.link+'" '+attrs+' >'+content+'</a>';
             };
         } else {
             elem = function(content, attrs) {
                 return '<span '+attrs+'>'+content+'</span>';
             };
         }
         $('#title h2').append(elem(talkie.talk, title));
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
