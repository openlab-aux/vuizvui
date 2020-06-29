{ fontsQuattrocentoLatin, fontsOpenSansLatin }:
''
/* latin */
@font-face {
    font-family: 'Quattrocento';
    font-style: normal;
    font-weight: 400;
    src: local('Quattrocento'), url(${fontsQuattrocentoLatin}) format('woff2');
    unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02BB-02BC, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2122, U+2191, U+2193, U+2212, U+2215, U+FEFF, U+FFFD;
}
/* latin */
@font-face {
    font-family: 'Open Sans';
    font-style: normal;
    font-weight: 300;
    src: local('Open Sans Light'), local('OpenSans-Light'), url(${fontsOpenSansLatin}) format('woff2');
    unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02BB-02BC, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2122, U+2191, U+2193, U+2212, U+2215, U+FEFF, U+FFFD;
}

body {
    font-family: 'Open Sans', sans-serif;
    font-size: 1em;
}

main {
    margin: 0 auto;
    width: 500px;
}

#title {
    text-align: center;
}
#title h1 {
    font-size: 4em;
    font-weight: 300;
    margin-bottom: 0em;
}
#title h2 {
    font-size: 1.5em;
    font-weight: 300;
    margin-top: 0;
}
#title h2 a {
    color: #000;
    text-decoration: none;
}
#title h2 a:hover {
    color: #333;
}
#title hr {
    width: 100px;
    border: 1px solid grey;
    margin-bottom: 2em;
}

p {
    font-family: 'Quattrocento', serif;
    text-align: justify;
    line-height: 1.5em;
}

#codeblock {
    text-align: center;
    font-family: monospace;
    font-size: 16px;
}
''
