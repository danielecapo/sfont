Font-Racket


The goal of this project is to produce a library to work with fonts in Racket.
The idea is that using the ability of DrRacket to draw images in the REPL a similar software can be used for teaching and making experiment with 'parametric' definitions of fonts in an interective environment.

At the moment it can be used to read and write fonts in the UFO format. It can (but it is not tested in a serious way) read from UFO 2 and UFO 3, covert UFO 2 to UFO 3 and UFO 3 to UFO 2.

It can export the font in a minimal type1 plain text format that can be used by the last version of the AFDKO to produce an otf font file, or converted in a pfa format with the command type1 (available with AFDKO).

Once 'loaded' the font is represented in Racket struct and drawn in the REPL. 

It can perform operations on fonts, using the idea of font math (see http://code.typesupply.com/wiki/FontMath). The operations are implemented on a different and simpler representation of the data, they works with contours, components, anchors and kerning pairs.
These operations can be used to implement interpolations.
While they aren't assured to always work they can be actually useful to make complex operations and exporting the result to UFO.

The other functionality that need to be developed more carefully is a system of macros used to produce parametric fonts. There is a first approximation of it in fontwriter.rkt.

To learn more about it, I suggest to read the files in the examples directory, they have comment that explain the usage.

The examples use two ufo files from the Adobe Source Sans Pro code that can be downloaded from github. If you want to run the example you need to get these files and rewrite the correct path to them.

Every comment, suggestion and critique is wellcome.



