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
They can associate a layer's name to a color and arbitrary informations stored in a lib (see also the
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

@defstruct*[advance ((width real?) (height real?))]{

The advance width and height of a glyph. Advance dictates where
the next glyph should be placed.
}
                                
@defstruct*[image ((filename string?) (matrix trans-mat?) (color (or/c color/c #f)))]{

An image is defined by the filename, a transformation matrix and a color
(if the color is false, then no color is specified).
}

@defstruct*[layer ((name name/c) 
                   (guidelines (listof guideline?))
                   (anchors (listof anchor?))
                   (contours (listof contour?))
                   (components (listof component?)))]{

A layer is used to store informations about contours, anchors, 
components and guidelines.

Transformations applied to a layer are applied to its content.}

@defthing[foreground 'public.default]
@defthing[background 'public.background]{

Names of foreground and background layers.}

@defstruct*[guideline ((pos vec?) 
                       (angle real?) 
                       (name (or/c string? #f)) 
                       (color (or/c color/c #f)) 
                       (identifier (or/c symbol? #f)))]

@defstruct*[anchor 
            ((pos vec?)
             (name string?) 
             (color (or/c color/c #f))
             (identifier (or/c symbol? #f)))]

@defstruct*[component ((base name/c) (matrix trans-mat?) (identifier (or/c symbol? #f)))]

@defstruct*[contour ((identifier (or/c symbol? #f)) (points (listof point?)))]

@defstruct*[point 
            ((pos vec?)
             (type (one-of/c 'move 'line 'offcurve 'curve 'qcurve))
             (smooth boolean?)
             (name (or/c string? #f))
             (identifier (or/c symbol? #f)))]

@subsection{Functions}

@defproc[(get-glyph (f font?) (g name/c)) (or/c glyph? #f)]{
                                                            
Produces the glyph named @emph{g} from the font @emph{f}. If @emph{f} hasn't a glyph with that name, returns false.}

@defproc[(get-glyphs (f font?) (gs (listof name/c))) (listof glyph?)]{
                                                            
Maps every name in @emph{gs} with the corresponding glyph in @emph{f} (if the glyph exists).}

@defproc[(remove-glyph (f font?) (g name/c)) font?]{
                                                            
Functionally remove (produces a new font) the glyph named @emph{g} from @emph{f}.}

@defproc[(insert-glyph (f font?) (g glyph?)) font?]{
                                                            
Functionally insert (produces a new font) the glyph @emph{g} in @emph{f}.}

@defproc[(map-glyphs [proc (-> glyph? any/c)] [f font?]  [#:sorted sorted boolean? #f]) (listof any/c)]
@defproc[(for-each-glyphs [proc (-> glyph? any/c)] [f font?]  [#:sorted sorted boolean? #f]) void?]
@defproc[(filter-glyphs [proc (-> glyph? boolean?)] [f font?]) (listof glyph?)]{                                                                              
Like @racket[map] @racket[for-each] and @racket[filter], but the procedures are applied 
to the glyphs in the given font. If @racket[sorted] 
is @racket[#t] in @racket[map-glyphs] and @racket[for-each-glyphs] the glyphs will be sorted by name before
the operation.}

@defproc[(font-glyphs-list [f font?]) (listof glyph?)]{
                                                            
Produces a list of the glyphs in @emph{f}.}

@defproc[(sort-glyph-list [gl (listof glyph?)] 
                          [#:key key (-> glyph? any/c) 
                                 (lambda (g) (symbol->string (glyph-name g)))]
                          [#:pred pred (-> any/c any/c boolean?) string<?])
         (listof glyph?)]{
                          
Produce a list of sorted glyphs. The way glyphs are sorted is controlled by 
@racket[key] and @racket[pred].}
                                                            
@defproc[(get-layer [g glyph?] [l name/c foreground]) layer?]{
                                                              
Produce the layer named @emph{l} in the given glyph, the default layer is @racket[foreground].}

@defproc[(set-layer [g glyph?] [new-layer layer?]) glyph?]{
                                                              
Functionally sets a layer in a glyph.}

@defproc[(map-layers [proc (-> layer? any/c)] [g glyph?]  [#:sorted sorted boolean? #f]) (listof any/c)]
@defproc[(for-each-layers [proc (-> layer? any/c)] [g glyph?]  [#:sorted sorted boolean? #f]) void?]{                                                                              

Like @racket[map] and @racket[for-each], but the procedures are applied 
to the layers in the given glyph. If @racket[sorted] 
is @racket[#t] the layers will be sorted by name before
the operation.}

@defproc[(decompose-glyph [f font?] [g glyph?]) glyph?]{
                                                        
Produce a glyph from the given glyph decomposing components to outline.
The 'context' from which the components (other glyphs) are retrieved is a font.}

@defproc[(decompose-font [f font?]) font?]{
                                                        
Produce a font from the given font decomposing all glyphs.}


@defproc*[([(glyph-bounding-box [g glyph?] [f font?]) bounding-box/c]
           [(glyph-bounding-box [g glyph?]) bounding-box/c])]{
                          
Produces the bounding box for the glyph, if a font is provided the bounding box
will take into account components, otherwise the bounding box will be 
the bounding box of contours only.}

@defproc[(font-bounding-box [f font?] [components boolean? #t]) bounding-box/c]{
                                                        
Produces the bounding box of the font, if @racket[components] is @racket[#t] components
are taken into account.}     
                        
@defproc*[([(get-sidebearings [g glyph?] [f font?]) (or/c (cons/c real? real?) #f)]
           [(get-sidebearings [g glyph?]) (or/c (cons/c real? real?) #f)])]{
                          
Produces the sidebearings of the glyph in a pair (or @racket[#f] if they can't be found), if a font is provided the sidebearings will take into account components.
The @racket[car] of the pair represents the left sidebearing, while the @racket[cdr] represents the right one.}
                          

                                         