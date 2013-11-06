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

@defproc[(string->color [s string?]) color/c]{
                                              
Produces a color from a string in the format @racket["r,g,b,a"].}

@defproc[(color->string [c color/c]) string?]{
                                              
Produces a string in the format @racket["r,g,b,a"] from a color.}

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

@subsubsection{Reading and writing UFOs}

@defproc[(read-ufo (file path-string?)) font?]{

Read the UFO file and produce a @racket[font].}

@defproc[(write-ufo (f font?) (file path-string?) (#:overwrite overwrite boolean? #t) (#:format format (list/c 2 3) 2))
         void?]{

Write the @racket[font] to @racket[file], if @racket[#:overwrite] is @racket[#f]
and the file already exists, an error is raised.
The optional keyword argument @racket[#:format] is used to save in a specific
UFO format (default is @racket[2]).}

@subsubsection{Inspecting glyphs and layers}

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

@subsubsection{Sidebearings and spacing properties}

@defproc*[([(get-sidebearings [g glyph?] [f font?]) (or/c (cons/c real? real?) #f)]
           [(get-sidebearings [g glyph?]) (or/c (cons/c real? real?) #f)])]{
                          
Produces the sidebearings of the glyph in a pair (or @racket[#f] if they can't be found), if a font is provided the sidebearings will take into account components.
The @racket[car] of the pair represents the left sidebearing, while the @racket[cdr] represents the right one.}
                          

@defproc*[([(intersections-at [g glyph?] [f font?] [h real?]) (listof vec?)]
           [(intersections-at [g glyph?] [h real?]) (listof vec?)])]{
                          
Produces a list of intersections between the outlines (if a font is provided it will take into account components)
and the horizontal line passing through (0, @racket[h]).}                                         
                                                         
@defproc*[([(get-sidebearings-at [g glyph?] [f font?] [h real?]) (or/c (cons/c real? real?) #f)]
           [(get-sidebearings-at [g glyph?] [h real?]) (or/c (cons/c real? real?) #f)])]{
                          
Like @racket[get-sidebearings] but the sidebearings are calculated from the leftmost and rightmost
intersections produced by @racket[intersection-at].}
                                                                                        
                          
@defproc*[([(set-sidebearings [g glyph?] [f font?] [left real?] [right real?]) glyph?]
           [(set-sidebearings [g glyph?] [left real?] [right real?]) glyph?])]{
                          
Produces a glyph with the given sidebearings (ie. if @racket[get-sidebearings] is applied to
the new glyph the result is @racket[(cons left right)]).}
                                                                              
@defproc*[([(set-sidebearings-at [g glyph?] [f font?] [left real?] [right real?] [h real?]) glyph?]
           [(set-sidebearings-at [g glyph?] [left real?] [right real?] [h real?]) glyph?])]{
                          
Produces a glyph with the given sidebearings at the given height (ie. if @racket[get-sidebearings-at] is applied to
the new glyph the result is @racket[(cons left right)]).}
                                                                                           
@defproc*[([(adjust-sidebearings [g glyph?] [f font?] [left real?] [right real?]) glyph?]
           [(adjust-sidebearings [g glyph?] [left real?] [right real?]) glyph?])]{
                          
Produces a glyph adding @racket[left] and @racket[right] to the sidebearings.}
                                                                                 
@subsubsection{Rounding coordinates}

@defproc[(font-round [f font?]) font?]
@defproc[(layer-round [l layer?]) layer?]
@defproc[(kerning-round [k kerning/c]) kerning/c]
@defproc[(glyph-round [g glyph?]) glyph?]
@defproc[(advance-round [a advance?]) advance?]
@defproc[(image-round [i image?]) image?]
@defproc[(anchor-round [a anchor?]) anchor?]
@defproc[(contour-round [c contour?]) contour?]
@defproc[(component-round [c component?]) component?]
@defproc[(guideline-round [g guideline?]) guideline?]
@defproc[(point-round [p point?]) point?]{

Round coordinates. This may be necessary for font editors that do not accept non-integer numbers for coordinates.}

@subsubsection{Contours, components, anchors and guidelines}

@defproc[(map-contours [proc (-> contour? any/c)] [o (or/c layer? glyph?)]) (listof any/c)]
@defproc[(for-each-contours [proc (-> contour? any)] [o (or/c layer? glyph?)]) void?]{

Like @racket[map] and @racket[for-each], but apply the procedure to every @racket[contour] in a @racket[layer] or,
if used with a @racket[glyph] to the @racket[foreground] layer of the glyph.}

@defproc[(map-components [proc (-> component? any/c)] [o (or/c layer? glyph?)]) (listof any/c)]
@defproc[(for-each-components [proc (-> component? any)] [o (or/c layer? glyph?)]) void?]{

Like @racket[map] and @racket[for-each], but apply the procedure to every @racket[component] in a @racket[layer] or,
if used with a @racket[glyph] to the @racket[foreground] layer of the glyph.}

@defproc[(map-anchors [proc (-> anchor? any/c)] [o (or/c layer? glyph?)]) (listof any/c)]
@defproc[(for-each-anchors [proc (-> anchor? any)] [o (or/c layer? glyph?)]) void?]{

Like @racket[map] and @racket[for-each], but apply the procedure to every @racket[anchor] in a @racket[layer] or,
if used with a @racket[glyph] to the @racket[foreground] layer of the glyph.}

@defproc[(map-guidelines [proc (-> guideline? any/c)] [o (or/c layer? glyph?)]) (listof any/c)]
@defproc[(for-each-guidelines [proc (-> guideline? any)] [o (or/c layer? glyph?)]) void?]{

Like @racket[map] and @racket[for-each], but apply the procedure to every @racket[guideline] in a @racket[layer] or,
if used with a @racket[glyph] to the @racket[foreground] layer of the glyph.}

@defproc[(map-points [proc (-> point? any/c)] [o (or/c layer? glyph?)]) (listof any/c)]
@defproc[(for-each-points [proc (-> point? any)] [o (or/c layer? glyph?)]) void?]{

Like @racket[map] and @racket[for-each], but apply the procedure to every @racket[point] in a @racket[contour].}

@defproc[(contour->bezier [c contour?]) bezier/c]{

Produces a cubic @racket[bezier/c] from a @racket[contour]. Line segments are transformed in cubic bezier segments
where the first control point is equal to the start point and the second control point is equal to the end point.}

@defproc[(bezier->contour [b bezier/c]) contour?]{

Produces a @racket[contour] from a @racket[bezier/c].}

@defproc[(component->outlines [c component?] [g glyph?]) (listof contour?)]{

Apply the transformation matrix of the component to the base glyph. Produces a list of contours.}

@defproc[(contour-open? [c contour?]) boolean?]{

True if the contour is open (ie. if the contour starts with a @emph{move} point.}

@defproc[(reverse-contour [c contour?]) contour?]{

Produces a contour with points in the reverse order.}

@defproc[(glyph-reverse-directions [g glyph?]) glyph?]{

Produces a glyph with contours reversed.}

@defproc[(glyph-correct-directions [g glyph?]) glyph?]{

Produces a glyph following the postscript convention.}

@subsubsection{kerning}

@defproc[(kern-groups2->3 [f font?]) font?]{

Produces a new font with kerning groups names following the UFO3 convention.}

@defproc[(kerning-group-names [f font?]) (cons/c (listof name/c) (listof name/c))]{

Produces the kerning groups names in a pair of lists. The @racket[car] is for left groups, the @racket[cdr] for right groups.}

@defproc[(valid-kerning-group-name? [g name/c] [side (or/c 'left 'right)]) boolean?]{

True if the name is a valid kerning group name for the given side. UFO3 requires left groups to start
with public.kern1. and right groups with public.kern2. .}

@defproc[(left-kerning-group? [g name/c]) boolean?]{

True if the name is a valid left kerning group name. UFO3 requires left groups to start
with public.kern1. .}

@defproc[(right-kerning-group? [g name/c]) boolean?]{

True if the name is a valid right kerning group name. UFO3 requires right groups to start
with public.kern2. .}

@defproc[(kerning-group? [g name/c]) boolean?]{

True if the name is a kerning group name.}

@defproc[(update-kerning-group-name [g name/c] [side (or/c 'left 'right)]) name/c]{

Produce a new name with the appropriate prefix.}

@defproc[(lookup-kerning-pair [f font?] [left name/c] [right name/c]) (values real? boolean?)]{

Find the kerning for the pair @racket[left] and @racket[right] in the font.
The second value returned is a boolean that signals if the kerning pair is defined or not.
If the kerning pair is not defined the kerning for that pair is always zero.}

@defproc[(kerning-value [f font?] [left name/c] [right name/c]) real?]{

Find the kerning for the pair @racket[left] and @racket[right] in the font 
(this is the first value prouced by @racket[lookup-kerning-pair]).
If the kerning pair is not defined the kerning for that pair is always zero.
}

@defproc[(map-kerning [proc (-> real? real?)] [k kerning/c]) kerning/c]{

Apply the procedure to every kerning pair, produce a new kerning table
with the updated kerning values.
}

@subsubsection{More}       

@defproc*[([(glyph-signed-area [g glyph?] [f font?] [sides natural-number/c]) real?]
           [(glyph-signed-area [g glyph?] [sides natural-number/c]) real?])]{
                          
Produces the area of the glyph (if font s provided components will be considered too). If the directions
of a contour is counterclockwise the 'signed' area will be positive, therefore if the sum of 'signed' areas for all contours
in the glyph is negative, contours direction is almost certainly wrong for postscript fonts.}
                                                                            
@defproc[(lowercase-stems [f font?]) real?]{

Produces the width of lowercase stems from the glyph @emph{n}, if  the font hasn't a glyph
named @emph{n} an error is raised.}

@defproc[(uppercase-stems [f font?]) real?]{

Produces the width of uppercase stems from the glyph @emph{H}, if  the font hasn't a glyph
named @emph{H} an error is raised.}

@defproc[(correct-directions [f font?]) font?]{

Produces a new font for which every glyph has a positive signed area.}

@defproc[(print-glyph [f font?] [gn name/c]) pict?]{

Print the font's glyph named @racket[gn] at the REPL. Actually, by evaluating an expression that produce a glyph,
the glyph is showed at the REPL, with @racket[print-glyph], however, the right vertical metrics of the fonts can be used.}