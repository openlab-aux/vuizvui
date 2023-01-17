#!/usr/bin/env python3
import os
import sys

from argparse import ArgumentParser

COLORS = {
    "k": (30, "Black"),
    "r": (31, "Red"),
    "g": (32, "Green"),
    "y": (33, "Yellow"),
    "b": (34, "Blue"),
    "p": (35, "Pink"),
    "c": (36, "Cyan"),
    "w": (37, "White"),
}

ESC = chr(27)


class ColorizeError(Exception):
    pass


class Color(object):
    def __init__(self, ident=None):
        """
        Initialize a color object, if no `ident` is given or it's invalid,
        the Color object represents "no color".
        """
        if ident is not None:
            spec = COLORS.get(ident.lower(), None)
        else:
            spec = None

        if spec is None:
            self.ident = None
            self.bold = False
            self.code = None
            self.name = "None"
        else:
            self.ident = ident
            self.code, self.name = spec

            if ident.isupper():
                self.bold = True
            else:
                self.bold = False

    @property
    def attrs(self):
        """
        A tuple consisting of the SGR attributes.
        """
        if self.ident is None:
            return ()

        if self.bold:
            return (1, self.code)
        else:
            return (self.code,)

    def sgr_attrs(self, *attrs):
        """
        Return the attributes specified by `attrs` formatted according
        to the CSI specification.
        """
        return ';'.join(map(lambda c: str(c), attrs))

    def sgr(self, *attrs):
        """
        Start Set Graphics Rendition
        Return the CSI escape code for `attrs`.
        """
        return "%s[%sm" % (ESC, self.sgr_attrs(*attrs))

    def sgr_start(self):
        """
        Start Set Graphics Rendition
        Return the CSI start escape code for the current color.
        """
        return self.sgr(*self.attrs)

    def sgr_stop(self):
        """
        Clear Set Graphics Rendition
        """
        return self.sgr()

    def apply(self, value):
        """
        Apply the current color to the string in `value`.
        """
        return "%s%s%s" % (self.sgr_start(), value, self.sgr_stop())

    def describe(self):
        """
        Return the description of the current color IN color :-)
        """
        fmt = "%c: <ESC>[%sm -> [%s]"
        return fmt % (
            self.ident,
            self.sgr_attrs(*self.attrs),
            self.apply(self.name)
        )

    def transform_to(self, new_color):
        """
        Return the CSI sequences needed to transform into `new_color`.
        """
        if self.ident is None and new_color.ident is not None:
            return new_color.sgr_start()
        elif self.ident is not None and new_color.ident is None:
            return self.sgr_stop()
        elif self.ident is None and new_color.ident is None:
            return ''
        elif self.code == new_color.code:
            if not self.bold and new_color.bold:
                return self.sgr(1)
            elif self.bold and not new_color.bold:
                return self.sgr(22)
            elif self.bold == new_color.bold:
                return ''
        else:
            if self.bold and new_color.bold:
                return new_color.sgr(new_color.code)

        return self.sgr_stop()+new_color.sgr_start()

    def __repr__(self):
        if self.bold:
            return "<Bold color %s>" % self.name.lower()
        else:
            return "<Color %s>" % self.name.lower()


def print_colortable():
    for ident in COLORS.keys():
        normal = Color(ident).describe()
        bold = Color(ident.upper()).describe()
        sys.stdout.write("%-35s%s\n" % (normal, bold))


def colorize_art(art, colmap):
    if len(art) != len(colmap):
        raise ColorizeError("Art and colormap differ in size!")

    no_color = Color()

    out = ""
    last_color = no_color
    for i, char in enumerate(colmap):
        color = Color(char)
        out += last_color.transform_to(color) + art[i]
        last_color = color

    last_color.transform_to(no_color)

    return out


def colorize_file(artfile, mapfile=None):
    if mapfile is None:
        mapfile = os.path.splitext(artfile)[0]+'.colmap'

    asciiart = open(artfile, 'r').read()
    colormap = open(mapfile, 'r').read()

    return colorize_art(asciiart, colormap)


if __name__ == "__main__":
    parser = ArgumentParser()

    table_or_merge = parser.add_mutually_exclusive_group(required=True)
    table_or_merge.add_argument(
        '-t', '--table', action='store_true', dest='table',
        help="Show color table and exit."
    )
    table_or_merge.add_argument(
        'artfile', nargs='?',
        help="ASCII art file to colorize"
    )

    parser.add_argument(
        'mapfile', nargs='?',
        help="The file with the color information."
             " (Default: artfile with .colmap suffix)"
    )

    options = parser.parse_args()

    if options.table:
        print_colortable()
    else:
        colorized = colorize_file(options.artfile, options.mapfile)
        sys.stdout.write(colorized)
