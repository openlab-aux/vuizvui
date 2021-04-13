// SPDX-License-Identifier: GPL-2.0-or-later
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>

enum file_type {
  PLAIN,
  MARKDOWN,
  HTML,
  CODE
};

void output_xml_escaped_char(char c, FILE *out) {
  switch(c) {
    case '&':
      fputs("&amp;", out);
      break;
    case '<':
      fputs("&lt;", out);
      break;
    case '>':
      fputs("&gt;", out);
      break;
    case '\'':
      fputs("&apos;", out);
      break;
    case '\"':
      fputs("&quot;", out);
      break;
    default:
      fputc(c, out);
      break;
  }
}

int main(int argc, char *argv[]) {
  bool about_filter;
  char *file;

  if(argc == 2) {
    about_filter = 0;
    file = argv[1];
  } else if(argc == 3 && strcmp(argv[1], "--about") == 0) {
    about_filter = 1;
    file = argv[2];
  } else {
    fprintf(stderr, "Usage: %s [--about] BASENAME\n", argv[0]);
    return 1;
  }

  char *extension = rindex(file, '.');
  enum file_type ft = PLAIN;

  if(extension != NULL) {
    extension++;

    if(about_filter && (strcmp(extension, "md") == 0 || strcmp(extension, "markdown") == 0)) {
      ft = MARKDOWN;
    } else if(about_filter && (strcmp(extension, "html") == 0 || strcmp(extension, "htm") == 0)) {
      ft = HTML;
    } else if(strcmp(extension, "txt") == 0) {
      ft = PLAIN;
    } else {
      ft = CODE;
    }
  } else {
    if(strcmp(file, "Makefile") == 0 || strcmp(file, "Doxyfile")) {
      ft = CODE;
    }
  }

  if(ft == PLAIN || ft == HTML) {
    char c;
    if(ft == PLAIN) {
      fputs("<pre>", stdout);
    }

    while((c = fgetc(stdin)) != EOF) {
      if(ft == HTML) {
        fputc(c, stdout);
      } else {
        output_xml_escaped_char(c, stdout);
      }
    }

    if(ft == PLAIN) {
      fputs("</pre>", stdout);
    }
  } else if(ft == MARKDOWN) {
    return execl("@lowdown@", "lowdown", NULL);
  } else if(ft == CODE) {
    return execl(
      "@chroma@", "chroma", "--filename", file,
      "--html", "--html-tab-width=2", "--html-only", "--html-inline-styles",
      "--style=lovelace",
      NULL
    );
  } else {
    return 1;
  }

  return 0;
}
