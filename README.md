# sfont


The goal of this project is to produce a library to work with fonts in Racket.
The idea is that using the ability of DrRacket to draw images in the REPL a similar software can be used for teaching and making experiment with 'parametric' definitions of fonts in an interective environment.

**This project can change in the future.**

You can see three screencasts here:

https://vimeo.com/69951725

https://vimeo.com/70019868

http://vimeo.com/73070195

At the moment it can be used to read and write fonts in the UFO format. It can (but it is not tested in a serious way) read from UFO 2 and UFO 3, covert UFO 2 to UFO 3 and UFO 3 to UFO 2.

It can export the font in a minimal type1 plain text format that can be used by AFDKO to produce an otf font file, or converted in a pfa format with the command type1 (available with AFDKO), however, this is no longer necessary since the latest version (september 2013) AFDKO supports UFOs.

Once 'loaded' the font is represented in a Racket struct and drawn in the REPL. 

It can perform operations on fonts, using the idea of font math (see http://code.typesupply.com/wiki/FontMath). The operations affect with contours, components, anchors and kerning pairs. To minimize incompatibility between fonts sfont provides some functions that remove line segments (they will be represented as 'curve' segments with aligned control points, sort contours, remove incompatible kerning pairs, components and anchors, etc.)
These operations can be used to implement interpolations.
While they aren't assured to always work they can actually be useful to make complex operations and exporting the result to UFO.

The other functionality that need to be developed more carefully is a system of macros used to produce parametric fonts. There is a first approximation of it in parametric/fontwriter.rkt.

To learn more about it, I suggest to read the files in the examples directory, they have comments that explain the usage.

The examples use two ufo files from the Adobe Source Sans Pro code that can be downloaded from github. If you want to run the example you need to get these files and rewrite the correct path to them, or you can use other UFOs.

Every comment, suggestion and critique is welcome.

## TO DO

Nice things that can be added to this library:

- Validation of UFOs
- Export to (and import from) fontforge sfd format
- Export and import glyphs in SVG

## Usage

### Functional style

Everything here is or should be immutable (at least, I tried to make everything immutable).
This means that operations like converting to UFO3 etc., will actually return a NEW font.

### Read and write UFOs

```
(require sfont)
```

To read a UFO:

```
(read-ufo "/path/to/your.ufo")
```

The font is represented as a structure (read ufo-def.rkt for more detail)
Kerning group names are autamatically converted using the UFO3 conventions (left kerning group names have to start with "public.kern1." and right group names have to start with "public.kern2").
It is assumed that UFOs2 use contours with one point to store anchors, the font structure will convert them in anchors. When saving they will be reconverted to contours (if saved as UFO2).
Glyphs are stored in layers (even for UFO2).

If `f` is your font, you can save it with `write-ufo`:

```
(write-ufo f "/path/to/your.ufo")
```

The normal behaviour is to overwrite everything, if you DON'T want to overwrite:

```
(write-ufo f "/path/to/your.ufo" #:overwrite #f)
```

By default the font is written in the UFO2 format (and it loses layers, guidelines, etc.), if you want to save it as UFO3 you can use the `#:format` keyword.

```
(write-ufo f "/path/to/your.ufo" #:format 3)
```

### Navigating the font

The module `sfont/navigator` defines three macros that can be used to access font data in an easier way.

```
(fref f
	[glyph 'a]
	[layer foreground]
	[contours @ 0]
	[point @ 2]
	pos
	x)
```

Find the x position of the third point in the first contour of the layer foreground in the glyph a.

If you need to (functionally) 'change' the same object you can use `fset`

```
(fset
	(f
	 [glyph 'a]
	 [layer foreground]
	 [contours @ 0]
	 [point @ 2]
	 pos
	 x)
	100)
```

If you want to update data with a function

```
(fupdate
	(f
	 [glyph 'a]
	 [layer foreground]
	 [contours @ 0]
	 [point @ 2]
	 pos
	 x)
	(lambda (x) (+ x 20)))
```
### Interpolations

The module `sfont/math` defines functions for interpolations.
```
(define-interpolable-fonts 
	[light light-font]
	[bold  bold-font]
	[wide  wide-font])
```

Defines three new fonts that can be interpolated (removing the glyphs that can't be made interpolables, and doing other things to maximize the compatibility.)

Now you can use arithmetic signs on them:

```
(+ (light (* (- bold light) 0.2)))
```
To further simplify operations it is possible to define a *space* with one of the fonts on the *origin*

```
(define-space s1 (light [bold wide]))
```
`define-space` creates two names `s1-bold` and `s1-wide` (the resulting fonts of `(- bold light)` and `(- wide light)`) that can be used with the space to write:

```
(s1 (* 0.2 s1-bold))
```
that is equivalent to the interpolation given above `(+ (light (* (- bold light) 0.2)))`, or something more complex like:


```
(s1 (+ (* 0.3 s1-bold) (0.7 s1-wide)))
```



### Spacing fonts

In spacing/space.rkt you have two macros `space` and `kern`.
The basic usage of space is

```
(space f
	a : 20 60
	b : 60 20
	...)
```

The form `a : 20 60` should be intuitive, the first is the glyph, after the slash you have the left and right sidebearings.
Sidebearings can be expressed in various ways:

`--` 
keeps the sidebearing unchanged

`(v h)` 
sets the sidebearing v at height h (like measurement line in FontLab)

`(/--/ v)` 
set the advance width to v

`(<-> v)`
adjust the sidebearing by v

You can mix the various forms in left and right sidebearings

`a : 30 (60 200)`
set the left sidebearing to 30, set the right sidebearing (measured at height 200) to 60

`z : (/--/ 300) 40`
set the advance width to 300 with the right sidebearing set to 40

`z : -- (/--/ 300)`
set the advance width to 300

`z : 40 (/--/ --)`
set the left sidebearing to 40 and keep the current advance width

You can define groups (but they are not added to the font groups dictionary):

```
(space f
	[groups
		(rounded-left '(c d e o q))]
	@ rounded-left : 30 --)
```

You need to place `@` in front of the group names in the spacing forms.

It is like

```
(space f
	c : 30 --
	d : 30 --
	e : 30 --
	o : 30 --
	q : 30 --)
```

Another way to do this is to group glyphs in parentheses in the spacing form:

```
(c d e o q) : 30 --
```	

To change a single glyph you can use `space-glyph`:
`(space-glyph g 20 (<-> -20))`
the left and right spacing forms are the same we have seen before.
If the glyph has components you have to pass the 'context' (the font) to `space-glyph` (because the actual curves of the base glyphs are stored in the font).
`(space-glyph (g f) 20 (<-> -20))`
	
### Kern macro

```
(kern fo
	[groups 
		(side1 [rounded_left '(o b p)])
		(side2 [diagonal_right '(v w y)])]
	@ rounded_left @ diagonal_right : -40
	o o : 20)
```

The kerning groups are added to the font groups with the prefixes public.kern1. and public.kern2., the kern form is simple enough (use `@` before groups, in that way the macro will add the correct prefix for you).

### Spacing Rules

In spacer you will also find the `define-spacing-rule` macro. It is used to produce a function for spacing fonts according to some rule; I've used it to define the functions `lowercase-tracy` and `uppercase-tracy` that apply the procedure described by W. Tracy to space letters.

#### lowercase-tracy function

This function is called with the font, the x-height of the font, the space applied to straight lowercase letters, the space applied to round letters, the mimimum space (for v, w, etc.) and, optionally two values used for adjusting the spacing for letters like c and l (the adjustments are used when Tracy says 'slightly more' etc.).

```
(lowercase-tracy f 500 100 40 10) 
```

#### uppercase-tracy function

The function is called with the font, the height of capitals, the space applied to H, the space applied to round letters, the minimum space for diagonal letters.

```
(upper-tracy f 700 140 70 11) 
```


### Macros for defining fonts

In parametric/fontwriter.rkt I've defined macros that can be used to produce fonts. You can browse the example directory to learn about them (in particular the file examples/fontwrite-square.rkt is commented and can be used as a guide).

My idea is that a parametric font should be represented as a function that relates an input (the *parameters*) to an output (the resulting font). In this way when we call the font-function we have a new font output and, if we can display the resulting font, we can easily explore our 'design space'.

The font macro comes in two format, you can use it without parameters and it will simply output a single font or you can define it with parameters and it will produce a procedure that can be called as explained above.








	

	


