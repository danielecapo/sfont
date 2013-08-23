sfont


The goal of this project is to produce a library to work with fonts in Racket.
The idea is that using the ability of DrRacket to draw images in the REPL a similar software can be used for teaching and making experiment with 'parametric' definitions of fonts in an interective environment.

You can see two screencasts here:
https://vimeo.com/69951725
https://vimeo.com/70019868

At the moment it can be used to read and write fonts in the UFO format. It can (but it is not tested in a serious way) read from UFO 2 and UFO 3, covert UFO 2 to UFO 3 and UFO 3 to UFO 2.

It can export the font in a minimal type1 plain text format that can be used by the last version of the AFDKO to produce an otf font file, or converted in a pfa format with the command type1 (available with AFDKO).

Once 'loaded' the font is represented in Racket struct and drawn in the REPL. 

It can perform operations on fonts, using the idea of font math (see http://code.typesupply.com/wiki/FontMath). The operations are implemented on a different and simpler representation of the data, they works with contours, components, anchors and kerning pairs.
These operations can be used to implement interpolations.
While they aren't assured to always work they can actually be useful to make complex operations and exporting the result to UFO.

The other functionality that need to be developed more carefully is a system of macros used to produce parametric fonts. There is a first approximation of it in fontwriter.rkt.

To learn more about it, I suggest to read the files in the examples directory, they have comments that explain the usage.

The examples use two ufo files from the Adobe Source Sans Pro code that can be downloaded from github. If you want to run the example you need to get these files and rewrite the correct path to them, or you can use other UFOs.

Every comment, suggestion and critique is welcome.

-------

Usage

Functional style

Everything here is or should be immutable (at least I tried to make everything immutable).
This means that operations like converting to UFO3 etc., will actually return a NEW font.

Read and write UFOs

(require "ufo.rkt")

To read a UFO:

(read-ufo "/path/to/your.ufo")

The font is represented as a structure (read ufo-def.rkt for more detail)
Kerning group names are autamatically converted using the UFO3 conventions (left kerning group names have to start with "public.kern1." and right group names have to start with "public.kern2").
It is assumed that UFOs2 use contours with one point to store anchors, the font structure will convert them in anchors. When saving they will be reconverted to contours (if saved as UFO2).
Glyphs are stored in layers (even for UFO2).

If your f is your font, you can save it with write-ufo:

(write-ufo f "/path/to/your.ufo")

The normal behaviour is to overwrite everything, if you DON'T want to overwrite:

(write-ufo f "/path/to/your.ufo" #:overwrite #f)

Convert between UFO2 and UFO3

(font->ufo3 f)
(font->ufo2 f)

So, to save as UFO3 you can:

(write-ufo (font->ufo3 f) "/path/to/your.ufo")

Navigating the font

You can use the normal way, i.e. structures field accessors, to get the various parts.
However since this can be annoying for a deeply nested data structure (I suppose the term is correct) like a font, sfont provides the "in", "seq" and "==>" macros (it is dubious, however, if it can be considered better):

The basic structure of in is:
(in object [field]) -> (object-type-field object)

for example

(in f [groups]) -> (font-groups f)

The other way of using it is
(in object [field f]) -> (f (object-type-field object))

(in object [field (lambda ...)]) -> ((lambda ...) (object-type-field object))

(in object [field (f . r)]) -> (f (object-type-field object) . r)

Some example:
(in f [layers first]) -> (first (font-layers f))

(in f [layers (lambda (ls) (map layer-name ls))]) -> ((lambda (ls) (map layer-name ls)) (font-layers f))

(in f [layers (list-ref 0)]) -> (list-ref (font-layers f) 0)

But you can add more field forms, the result of the previous operation will be passed to the second, etc:

(in (get-glyph f 'a) [contours first] [points (list-ref 2)] [pos] [x])

(in (get-glyph f 'a) [contours first]) -> this will get the first contour of "a" that will be passed to

then [points (list-ref 2)] will get the third point in the contour
then [pos] will get the position vector of the point
finally [x] will get the x coordinate.

This macro uses the ==> macro (it exists a similar thing in clojure) to chain expressions.

The macro seq let's you access data in another way:

(seq f) -> will returns an hashtable of glyphs in the foreground layer
(seq f 'a) -> glyph "a" of the foreground layer
(seq f 'a 0) -> the first contour of "a"
(seq f 'a 0 2) -> the third point of this contour


Spacing fonts

In spacer.rkt you have two macros "space" and "kern".
The basic usage of space is

(space f
	a / 20 60
	b / 60 20
	...)
	
The form is a / 20 60, should be intuitive, the first is the glyph, after the slash you have left and right sidebearings.
Sidebearings can be expressed in various ways:


