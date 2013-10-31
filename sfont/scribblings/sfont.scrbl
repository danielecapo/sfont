#lang scribble/manual
@(require scribble/eval
          "../main.rkt")

@title{Sfont: play with fonts in Racket}

@author[(author+email "Daniele Capo" "capo.daniele@gmail.com")]

@(require (for-label racket
                     racket/contract/base
                     slideshow/pict-convert
                     sfont))



Sfont is a collection for writing, reading and modifying fonts in the 
@link["http://unifiedfontobject.org/"]{UFO format}.


@section{Font structures and contracts}

@defmodule[sfont]

@defstruct*[font ([fontinfo fontinfo/c]
                  [groups groups/c]
                  [kerning kerning/c]
                  [features features/c]
                  [glyphs (or/c (listof glyph?) 
                                (hash/c name/c glyph? #:immutable #t))]
                  [layers (or/c (listof layer-info?) 
                                (hash/c name/c layer-info? #:immutable #t))]
                  [lib lib/c]
                  [data data/c]
                  [images images/c])]{
A structure type for fonts.                                     
Font structures are @racket[pict-convertible?] and are printed at the REPL.
The way they are printed depends on the parameters @racket[display-size], @racket[display-text] 
and @racket[show-kerning?].

Glyphs and layers are stored internally in hash tables so they can be easily accessed by name,
however the guard of font accept list of glyphs and layer-infos and transform them in hash tables.}

@defparam[display-size size natural-number/c]{
                                              
The size of the font.}

@defparam[display-text text (listof (listof symbol?))]{
                                                       
The text to be printed. Every line is represented as a list of symbols (the name of glyphs). If a glyph is not in the font it is not shown.}

@defparam[show-kerning? show boolean?]{
                                       
Determine wheter kerning is applied to the sample text.}

@defthing[name/c flat-contract?]{
                                 
A name is a symbol.}

@defthing[fontinfo/c flat-contract?]{
                                     
Fontinfo is an immutable hash table whone keys are @racket[name/c].}

@defthing[lib/c flat-contract?]{
                                
Libs are immutable hash table whose keys are  @racket[name/c].}

@defthing[groups/c flat-contract?]{
                                
Groups are represented in an immutable hash table whose keys and values 
are @racket[name/c] and list of @racket[name/c].}


@defthing[kerning/c flat-contract?]{
                                
Kerning table is represented in an immutable hash table;
keys are @racket[name/c] and represent the first item in the
kerning pair. Values are immutable hash tables with @racket[name/c]
as key and a real number as value.
For example:
@racketblock[
(hash 'v
      (hash 'o -20
            'e -18))]
specifies a kerning table where the value the kerning between v and o is -20
and the kerning between v and e is -18.
}
      
@defthing[features/c flat-contract?]{
                                
Features are stored in a string.}

@defthing[data/c flat-contract?]

@defthing[images/c flat-contract?]{

Data and images (defined in UFO3 specifications) are normally represented by paths
to files.}

@defthing[color/c flat-contract?]{
                                  
A color is a list of four numbers between 0 and 1. The first three value are th rgb values,
the fourth one is the alpha value (transparency).}

@defstruct*[layer-info ((name name/c) 
                        (color (or/c color/c #f))
                        (lib lib/c))]{

Layer infos are used to store informations about layers at font level.
They can associate to the layer's name a color and arbitrary informations in a lib (see also the
@link["http://unifiedfontobject.org/versions/ufo3/glyphs.html#layerinfo"]{UFO specifications}.)

If a layer is added to a glyph, the respective layer-info has to be added to the font layers.}

@defstruct*[glyph ((name name/c) 
                   (advance advance?)
                   (unicodes (listof natural-number/c))
                   (note (or/c string? #f))
                   (image (or/c image? #f))
                   (layers (or/c (listof layer?) (hash/c name/c layer? #:immutable #t)))
                   (lib lib/c))]{
                                 
A structure type for glyphs. Layers are stored in an hash tables (but,
like glyphs in fonts can be passed as lists to the constructor).

Transformations can be applied to glyphs, however only @racket[scale] will
affect the advance width and height of the glyph.}