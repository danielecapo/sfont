#lang scribble/manual
@(require scribble/eval
          "../main.rkt"
          "../parametric/fontwriter.rkt")

@title{Sfont: play with fonts in Racket}

@author[(author+email "Daniele Capo" "capo.daniele@gmail.com")]

@(require (for-label racket
                     racket/contract/base
                     slideshow/pict-convert
                     sfont
                     sfont/space
                     sfont/geometry
                     sfont/navigator
                     sfont/parametric/fontwriter
                     sfont/utilities
                     sfont/windows))



Sfont is a collection for writing, reading and modifying fonts in the 
@link["http://unifiedfontobject.org/"]{UFO format}.

@table-of-contents[]

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

@defproc[(font->pict [f font?]) pict?]{
                                       
Produces a @racket[pict].}                                       

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
The hash table don't have a clear order, but when the font is written with @racket[write-ufo]
the kerning pairs are sorted with @racket[sorted-kerning-list].
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
                                
@defproc[(glyph->pict [g glyph?]) pict?]{
                                       
Produces a @racket[pict].}                                

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
If the kerning pair is not defined the kerning for that pair is always zero.
The kerning pair lookup doesn't check possible conflicts, it always returns the
first value found (first glyphs pairs, then group glyph pairs, finally group group pairs).
}

@defproc[(kerning-value [f font?] [left name/c] [right name/c]) real?]{

Find the kerning for the pair @racket[left] and @racket[right] in the font 
(this is the first value prouced by @racket[lookup-kerning-pair]).
If the kerning pair is not defined the kerning for that pair is always zero.
}

@defproc[(map-kerning [proc (-> real? real?)] [k kerning/c]) kerning/c]{

Apply the procedure to every kerning pair, produce a new kerning table
with the updated kerning values.}

@defproc[(sorted-kerning-list [k kerning/c]) (listof (cons/c name/c (listof (cons/c name/c real?))))]{

Produces an association list that repressents the kerning table sorted according sorted 
according the kerning levels hierarchy defined by the UFO specifications: the first level
is for group combinations, then group-glyph (or glyph-group) combinations, finally glyph combinations;
this means that group names precede glyph names.
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



@section{Spacing fonts}

@defmodule[sfont/space]

This module define functions and macros for spacing fonts. 

@defform/subs[(space font-expr maybe-groups spacing-form ...)
              ([maybe-groups (code:line)
                             (code:line [groups (group-name group) ...])]
               [spacing-form (code:line glyph-name : side side)
                             (code:line \@ group-name : side side)
                             (code:line (glyph-name ...) : side side)]
               [side (code:line --)
                     (code:line (/--/ advance-width))
                     (code:line (<-> adjustment))
                     (code:line (sidebearing height))
                     (code:line sidebearing)])]{

This syntax form is designed to define the spacing of a font. The result is a new font with the spacing applied.
One example should clarify how it works:
@racketblock[(space font-to-be-spaced
                    [groups (group1 '(h i l m n))
                            (group2 '(o e c d))]
                    v        : 10 10
                    f        : 80 (200 100)
                    a        : -- (/--/ 400)
                    k        : (/--/ 400) --
                    (s z)    : (<-> 20) (<-> 20)
                    \@ group1 : (80 200) (80 100)
                    \@ group2 : 40 --)]

Two groups are defined (group1 and group2) and they are added to the font groups field.

@racket[v : 10 10]: means set both sidebearings of v to @racket[10]

@racket[f : 80 (200 100)]: means set the left sidebearings of f to @racket[80] while
the right sidebearing is set to @racket[200] at height @racket[100]

@racket[a : -- (/--/ 400)]: means leave the left sidebearings of a untouched and set its advance width to @racket[400]

@racket[k : (/--/ 400) --]: means leave the right sidebearings of k untouched and set its advance width to @racket[400]

@racket[(s z) : (<-> 20) (<-> 20)]: defines an inline group (s and z) that won't be added to the font groups.
and adjust the left and right sidebearings of both glyphs by @racket[20] units.

@racket[\@ group1 : (80 200) (80 100)]: applies the spacing rules to every glyph in group1,
The left sidebearing is set to @racket[80] at height @racket[200], while the right sidebearing is set to @racket[80] at height @racket[100]

@racket[\@ group2 : 40 --]: sets the left sidebearing of every glyph in group2 to @racket[40].

The various forms can be combined freely.}


@defform*[((space-glyph glyph-expr side side)
           (space-glyph (glyph-expr font-expr) side side))]{

Produce a new glyph with the spacing applied, the syntax of @racket[side] is the same used in @racket[space].
The second form, that specify a font, takes into account components.}

@defform/subs[(kern font expr maybe-groups kern-form ...)
              ([maybe-groups (code:line)
                             (code:line [groups (side1 [group-name group] ...)
                                                (side2 [group-name group] ...)])]
               [kern-form (code:line glyph1 glyph2 : kern)
                          (code:line \@ group-name glyph2 : kern)
                          (code:line glyph1 \@ group-name : kern)
                          (code:line \@ group-name \@ group-name : kern)])]{

This syntax can be used to define the kerning table of a font:
@racketblock[(kern font-to-be-kerned
                   [groups
                    (side1 [curved '(d e c o)])
                    (side2 [diag   '(v w y)])]
                   \@ roundR \@ diagL : -20
                   k o : -16)]}

@defproc[(lowercase-tracy [f font?] [xh real?] [n real?] [o real?] [min real?] [c-adj real? 0.80] [l-adj 1.05]) font?]{
                                                                                                                 
Produce a new font applying the rule described in W.Tracy's book "Letters of Credit"
for the lowercase latin alphabet. In this rule the designer should set the left sidebearing of @bold{n},
the sidebearings of @bold{o} and the minimum space to be applied to diagonal letters.
Moreover, Tracy says that some space should be 'slightly less or 'slightly more, this is
captured by the two optional arguments.
The first argument (@racket[xh]) is the x-height of the the font.}

@defproc[(uppercase-tracy [f font?] [caps real?] [h real?] [o real?] [min real?]) font?]{

Produces a new font applyng W.Tracy's rule for uppercase latin alphabet.
The designer have to set the spacing for @bold{H}, @bold{O} and a minimum
for uppercase diagonal letters.
The first argument is the height of uppercase letters.}

@defform[(define-spacing-rule name (arg ...) 
           (locals ...) 
           maybe-groups 
           spacing-form ...)]{
                                                                              
Define a new procedure @racket[name] with the arguments defined in @racket[arg ...]
locals are a list of bindings. The rest is like the @racket[space] macro.
The procedure applies the spacing to the font.
To make an example, this is a part of the definition of @racket[lowercase-tracy]:

@racketblock[(define-spacing-rule
  lowercase-tracy 
  [xh n o min [c-adj 0.80] [l-adj 1.05]]
  ([mid (/ xh 2)]
   [a n]
   [b (floor (* 0.9 n))]
   [c (floor (* l-adj n))]
   [d min]
   [e o]
   [f (floor (* c-adj o))])
  a : -- (b mid)
  b : (a mid) e
  c : e f
  d : e (a mid)
  e : e f
  f : -- --
  g : -- --
  h : (c mid) (b mid)
  ...)]
}
 
@section{Accessing and modifying data}

@defmodule[sfont/navigator]
@deftogether[(@defform[(fref ref-form ...)]
               @defform[(fset [ref-form ...] value)]
               @defform[(fupdate [ref-form ...] update-proc)])]{

These macros define another way to access the structures defined above.
For example
@racketblock[(fref f 
                   [glyph 'a] 
                   [layer foreground]
                   [contours \@ 2]
                   [points \@ 3]
                   pos
                   x)]
produces the @racket[vec-x] of @racket[point-pos] etc.
The form @racket[(fref f [glyph 'a])] is like calling @racket[(get-glyph f 'a)].

The form @racket[(fref g [layer foreground])] is like calling @racket[(get-layer g foreground)].

The form @racket[(fref l [contours \@ 2])] gets the third contour of the layer, 
the @racket[\@] can be used when the result is a sequence, to refer to a specific element in the sequence.

Setting a value is similar:

@racketblock[(fset [f 
                    [glyph 'a] 
                    [layer foreground]
                    [contours \@ 2]
                    [points \@ 3]
                    pos
                    x]
                   200)]
The first part between brackets uses the same conventions seen above, the second part is the new value.

The last form @racket[fupdate] update a value with a procedure. 
@racketblock[(fupdate [f 
                       [glyph 'a] 
                       [layer foreground]
                       [contours \@ 2]
                       [points \@ 3]
                       pos
                       x]
                      (lambda (x) (+ x 100)))]

The effect, here, is shifting a point rightward.
}
 
@section{Interpolations and font math}                             
@defmodule[sfont/math]

With this module it is possible to perform interpolations between fonts using the idea of 'glyph math' or 'font math'
(see @link["http://www.robofab.org/howto/glyphmath.html"]{Robofab glyphmath} or 
@link["https://github.com/typesupply/fontMath"]{Tal Leming's FontMath}).
The idea is that glyphs can be treated like vectors defining two basic operations: sum and scalar multiplication,
obviously the glyphs should be 'compatible' (the should have the same number of points).
In the same way we can extends the same abstraction with fonts (they also have to be compatible).
With this idea we can perform an interpolation between two fonts in a simple way: @racket[(+ a (* (- b a) f))]
where @racket[a] and @racket[b] are fonts and @racket[f] is a number.

The operations affect not only contours but also anchors, components, and font metric informations.
The operation of making two or more fonts compatible can be tedious, therefore the module define
functions and macros to do this:

@defproc[(get-interpolable-fonts [fs font?] ...) (listof font?)]{
                                                                 
Produces a list of mutually interpolable fonts. The process will
@itemlist[@item{keep only common glyphs}
          @item{transform line segments in curve segments (with control points at extrema)}
          @item{sort contours and points inside contours}
          @item{remove components or anchors if they are not in the corresponding glyphs}
          @item{sort components and anchors}
          @item{keep only common font informations}]}

@defform[(define-interpolable-fonts (id f) ...)]{
                                                 
Transforms every @racket[font] @racket[f] like @racket[get-interpolable-fonts]
and binds them to the corresponding @racket[id].

@racketblock[(define-interpolable-fonts 
               (light-c light-font)
               (bold-c  bold-font)
               (wide-c  wide-font))]

Now @racket[light-c], @racket[bold-c] and @racket[wide-c] are interpolables fonts.}

@defproc[(+ [o fontmath-object/c] [os fontmath-object/c] ...) fontmath-object/c]
@defproc[(* [o fontmath-object/c] [os fontmath-object/c] ...) fontmath-object/c]
@defproc[(- [o fontmath-object/c] [os fontmath-object/c] ...) fontmath-object/c]
@defproc[(/ [o fontmath-object/c] [os fontmath-object/c] ...) fontmath-object/c]{
                                                                                 
Redefine arithmethic operations.}

@defthing[fontmath-object/c flat-contract?]{
                                            
A @racket[fontmath-object/c] can be:  
@itemlist[@item{a @racket[font]}
          @item{a @racket[glyph]}
          @item{a @racket[vec]}
          @item{a @racket[bezier/c]}
          @item{a @racket[real?]}]
}

With the mathematical abstraction we are using we can express something more than 
interpolations. For example we can build a bold-wide from a light a bold and a wide.

@racketblock[(define bold-wide 
               (+ light-c
                  (- bold-c light-c)
                  (- wide-c light-c)))]

To make this simpler Sfont has a macro that define an interpolation 'space'
where one can simply write:

@racketblock[(define bold-wide
               (sp1
                (+ sp1-bold-c sp1-wide-c)))]

@defform[(define-space id (origin-id [font-id ...]))]{
                                                
Defines a space named @racket[id] that uses the font @racket[origin-id] as its
origin.
The 'space' is a procedure that produces a font.

@racketblock[(define-space sp1 (light-c [bold-c wide-c]))]

This line will define a procedure @racket[sp1] and two fonts: @racket[sp1-bold-c]
(obtained with @racket[(- bold-c light-c)]) and @racket[sp1-wide-c] (obtained with
@racket[(- wide-c light-c)]) that can be used like the example above.

In this 'space' the interpolations between light and bold can be expressed with

@racket[(sp1 (* sp1-bold-c 0.6))] 

that is equivalent to:

@racket[(+ light-c (* (- bold-c light-c) 0.6))] .} 
                                                  
@defproc[(x-> [o geometric?]) geometric?]
@defproc[(y-> [o geometric?]) geometric?]{
                                               
Projections of the object on the x and y axes. 
These projections can be useful to obtain the so-called 
@link["http://www.lucasfonts.com/about/interpolation-theory/"]{'anisotropic' interpolations.}

@racketblock[(sp1 (+ (* (x-> sp1-bold-c) 0.6)
                     (* (y-> sp1-bold-c) 0.4)))]

It will increase contrast.
}



@section{Geometry}

@defmodule[sfont/geometry]

This is the module for vector operations, transformations and bezier curves.

@defparam[precision p real?]{A parameter that controls rounding.}

@defproc[(approx [n real?]) real?]{
                                   
Approximate the numbers with the precision defined by @racket[precision].}                                   

@defstruct*[vec ([x real?] 
                 [y real?])]{
                                       
A structure that represents a 2D vector. A @racket[vec] implements the @racket[gen:geometric] interface.}
                            
@defstruct*[vec3 ([x real?] 
                  [y real?]
                  [z real?])]{
                                       
A structure that represents a 3D vector.}
                             
                             
@defproc[(vec3->vec [v vec3?]) vec?]
@defproc[(vec->vec3 [v vec?]) vec3?]{Conversion between 3D and 2D vcectors.}

@defstruct*[trans-mat ((x real?) 
                       (xy real?) 
                       (yx real?)
                       (y real?)
                       (x-offset real?)
                       (y-offset real?))]{

A transformation matrix. A @racket[trans-mat] implements the @racket[gen:geometric] interface.}

@defproc[(geometric? [o any/c]) boolean?]{
                                          
A predicate for structures that implement the generic group @racket[gen:geometric]}

@defproc[(transform [o geometric?] [m trans-mat?]) geometric?]{
                                                               
Applies the trasformation matrix to the object.}

@defproc[(translate [o geometric?] [x real?] [y real?]) geometric?]{
                                                               
Applies the translation to the object.}

@defproc[(rotate [o geometric?] [angle real?]) geometric?]{
                                                               
Applies the rotation to the object.}

@defproc[(scale [o geometric?] [fx real?] [fy real? fx]) geometric?]{
                                                               
Scales the object.}

@defproc[(skew-x [o geometric?] [angle real?]) geometric?]{
                                                               
Applies a shear transformation to the object.}

@defproc[(skew-y [o geometric?] [angle real?]) geometric?]{
                                                               
Applies a shear transformation to the object.}

@defproc[(reflect-x [o geometric?]) geometric?]{
                                                               
Reflects the object around the x axis.}

@defproc[(reflect-y [o geometric?]) geometric?]{
                                                               
Reflects the object around the y axis.}

@defproc[(vec= [v1 vec?] [v2 vec?]) boolean?]{
        
True if the x and y coordinates of the vectors are equal.}                                     


@defproc[(vec-approx= [v1 vec?] [v2 vec?]) boolean?]{
        
True if the x and y coordinates of the vectors are approximately equal (using the @racket[precision] parameter).}

@defproc[(list->vec [l (list/c real? real?)]) vec?]{
        
Produces a @racket[vec] from a list of two numbers.}

@defproc[(vec->list [v vec?]) (list/c real? real?)]{
        
Produces a list of two numbers from a @racket[vec].}

@defproc[(vec-length [v vec?]) (and/c real? (not/c negative?))]{
        
Produces the length of the vector.}

@defproc[(vec-angle [v vec?]) real?]{
        
Produces the angle of the vector.}

@defproc[(vec+ [v1 vec?] [v2 vec?]) vec?]{
        
Sum two vectors.}

@defproc[(vec- [v1 vec?] [v2 vec?]) vec?]{
        
Subtract two vectors.}

@defproc[(vec* [v1 vec?] [n real?]) vec?]{
        
Scalar multiplications.}

@defproc[(vec/ [v1 vec?] [n real?]) vec?]{
        
Scalar division.}

@defproc[(aligned? [v1 vec?] [v2 vec?] [v3 vec?]) boolean?]{
                                                               
True if the vectors @racket[v1], @racket[v1] and @racket[v1] are aligned.}

@defproc[(translation-matrix (x real?) (y real?)) trans-mat?]{
                                                               
Produces a translation matrix.}

@defproc[(rotation-matrix (angle real?)) trans-mat?]{
                                                               
Produces a rotation matrix.}

@defproc[(scale-matrix [fx real?] [fy real? fx]) trans-mat?]{
                                                               
Produces a scale matrix.}

@defproc[(shear-matrix [x real?] [y real?]) trans-mat?]{
                                                               
Produces a shear matrix.}

@defproc[(trans-mat* [m1 trans-mat?] [m2 trans-mat?]) trans-mat?]{
                                                                  
Multiplies two transformation matrices.}

@defproc[(trans-mat-vec* [m trans-mat?] [v vec3?]) vec3?]{
                                                                  
Multiplies the translation matrix and the 3D vector.}

@defproc[(dot-prod [v1 vec?] [v2 vec?]) real?]{
                                                                  
Produces the dot product of two 2D vectors.}

@defproc[(dot-prod-3 [v1 vec3?] [v2 vec3?]) real?]{
                                                                  
Produces the dot product of two 3D vectors.}

@defproc[(cross-prod-2d [v1 vec?] [v2 vec?]) real?]{
                                                                  
The third scalar component of the cross product.}

@defproc[(segment-intersection [v1 vec?] [v2 vec?] [v3 vec?] [v4 vec?]) (or/c vec? #f)]{
                                                                  
The intersection (if any) between the segments v1-v2 and v3-v4.}

@defproc[(signed-area [v1 vec?] [v2 vec?]) real?]{
                                                                  
The signed area of the triangle formed by the two vectors. Positive if the angle between vectors is counter-clockwise.}

@defproc[(signed-polygonal-area [lov (listof vec?)]) real?]{
                                                                  
The signed area of the polygon formed by the list of vectors. Positive if counter-clockwise.}

@defproc[(intersect-hor [h real?] [v1 vec?] [v2 vec?]) (or/c vec? #f)]{
                                                                       
Produces the intersection (if any) between the horizontal line passing from (0, h)
and the line segment v1-v2.}

@defproc[(intersect-vert [v real?] [v1 vec?] [v2 vec?]) (or/c vec? #f)]{
                                                                       
Produces the intersection (if any) between the vertical line passing from (v, 0)
and the line segment v1-v2.}

@defproc[(pass-through-hor? [h real?] [v1 vec?] [v2 vec?]) boolean?]{
                                                                       
True if the line segment v1-v2 intersects  horizontal line passing from (0, h).}

@defproc[(pass-through-vert? [v real?] [v1 vec?] [v2 vec?]) boolean?]{
                                                                       
True if the line segment v1-v2 intersects the vertical line passing from (v, 0).}

@subsection{Bezier Curves}

In Sfont a 'bezier curve' is a list of @racket[vec], a segment is a bezier curve
with only two 'on-curve' points.


@defthing[bezier/c flat-contract?]
@defthing[segment/c flat-contract?]
@defthing[cubic-bezier/c flat-contract?]
@defthing[cubic-segment/c flat-contract?]
@defthing[closed-bezier/c flat-contract?]

@defproc[(closed? [b bezier/c]) boolean?]{
                                          
True if the first and last points coincide.}

@defproc[(segments [b bezier/c] [o natural-number/c 3]) (listof segment/c)]{
                                                                            
'Explodes' a curve in a list of segments. The optional arguments declares the 'order' of the bezier curve
(basically: the number of control points for each segments plus one, cubic beziers have two points, quadratic one, etc.}

@defproc[(on-curve-points [b bezier/c] [o natural-number/c 3]) (listof vec?)]{
                                                                            
Produce a list of points removing all the control points from the bezier.}

@defproc[(off-curve-points [b bezier/c] [o natural-number/c 3]) (listof vec?)]{
                                                                            
Produce a list of control points of the bezier curve.}

@defproc[(end-points [b bezier/c]) (cons/c vec? vec?)]{
                                                                            
Produce a pair with the first and last point of the curve.}s

@defproc[(line-segment? [s segment/c]) boolean?]{
                                                 
True if the segment represents a line (ie. the points are aligned)}

@defproc[(canonical-line-segment [s cubic-segment/c]) cubic-segment/c]{
                                                                       
Produces a cubic line segment where control points are placed at the extrema.}

@defproc[(split [s segment/c] [t (real-in 0 1)]) (values segment/c segment/c)]{
                                                                       
Splits the segment in two parts. If @racket[t] is 0 or 1 the first/second half is an empty list.}

@defproc[(split-at-point [s segment/c] [v vec?]) (values segment/c segment/c)]{
                                                                               
Like @racket[split] but try to split the curve near the point provided.}                                                                               

@defproc[(join-beziers [b1 bezier/c] [b bezier/c] ...) bezier/c]{
                                                                       
Concatenate bezier curves. If the last and first points of two consecutive curves are not @racket[vec-approx=] an error is raised.}

@defproc[(point-at [s segment/c] [t (real-in 0 1)]) vec?]{
                                                          
Find the point on a segment at 'time' t.}                                                          

                                         
@defproc[(polygonize-segment [s segment/c] [n natural-number/c]) (listof vec?)]{
                                                          
Transform the segment in a polygon with @racket[n] sides.}   
                                                          
@defproc[(end-points-bounding-box [s segment/c]) bounding-box/c]{
                                                          
Produces the bounding box of the line segment that joins the endpoints of the bezier segment.}

@defproc[(end-points-at-extrema? [s segment/c]) boolean?]{
                                                          
True if the bounding-box of the segment and @racket[end-points-bounding-box] are the same.} 
                                                                                           
@defproc[(segment-bounding-box [s segment/c]) bounding-box/c]{
                                                          
Produces the bounding-box of the segment.}

@defproc[(bezier-bounding-box [b bezier/c] [o natural-number/c 3]) bounding-box/c]{
                                                          
Produces the bounding-box of the bezier curve.}

@defproc[(bezier-signed-area [b bezier/c] [o natural-number/c 3] [s natural-number/c 200]) real?]{
                                                          
The area of the bezier curve of order @racket[o]. 
The curve will be trasformed in a polygon, @racket[s] controls in how many sides every segment is divided.
Positive if the curve is counter-clockwise.}

@defproc[(bezier-area [b bezier/c] [o natural-number/c 3] [s natural-number/c 200]) (and/c real? positive?)]{
                                                          
The absolute value of @racket[bezier-signed-area].}

@defproc[(clockwise? [b bezier/c]) boolean?]{
                                   
True if the curve is clockwise.}

@defproc[(clockwise [b bezier/c]) bezier/c]{
                                   
Reverses the curve if it isn't clockwise.}

@defproc[(cubic-bezier-intersections [b1 cubic-bezier/c] [b2 cubic-bezier/c]) (listof vec?)]{
                                                                                 
Produces a list of intersections between the two cubic bezier curves.}

@defproc[(cubic-segments-intersections [s1 cubic-segment/c] [s2 cubic-segment/c]) (listof vec?)]{
                                                                                 
Produces a list of intersections between the two cubic bezier segments.}

@defproc[(line-segment-intersections [l segment/c] [s segment/c]) (listof vec?)]{
                                                                                 
Produces a list of intersections between the line segment and the segment.}

@defproc[(segment-intersect-hor [h real?] [s segment/c]) (listof vec?)]{
                                                                        
Produces a list of intersections between the bezier segment and a the horizontal line y = h.}

@defproc[(segment-intersect-vert [v real?] [s segment/c]) (listof vec?)]{
                                                                        
Produces a list of intersections between the bezier segment and a the vertical line x = v.}

@defproc[(bezier-intersect-hor [h real?] [s segment/c]) (listof vec?)]{
                                                                        
Produces a list of intersections between the bezier curve and a the horizontal line y = h.}

@defproc[(bezier-intersect-vert [v real?] [s segment/c]) (listof vec?)]{
                                                                        
Produces a list of intersections between the bezier curve and a the vertical line x = v.}

@defproc[(bezier-boundaries-hor [h real?] [s segment/c]) bounding-box/c]{
                                                                        
Produces the bounding box of the intersections between the bezier curve
and a the horizontal line y = h.}

@defproc[(point-inside-bezier? [v vec?] [b closed-bezier/c]) boolean?]{
                                                                        
True if the point is inside the bezier curve.}

@defproc[(bezier->path [b cubic-bezier/c] [path (is-a?/c dc-path%)]) (is-a?/c dc-path%)]{
                                                                        
Write the bezier to a @racket[dc-path%].}

@defproc[(print-beziers [b cubic-bezier/c] ...) pict?]{
                                                       
Print a graphic representation of the cubic bezier curves.}                                                       

@subsubsection{Boolean operations}

@defproc[(bezier-subtract [b1 closed-bezier/c] [b2 closed-bezier/c]) (listof closed-bezier/c)]{
                                                                        
Subtract the second closed bezier curve from the first, produces a list of closed bezier curves.}

@defproc[(bezier-union [b1 closed-bezier/c] [b2 closed-bezier/c]) (listof closed-bezier/c)]{
                                                                        
Add the second closed bezier curve to the first, produces a list of closed bezier curves
(if the curves don't intersect, for example, the two original curves are returned).}

@defproc[(bezier-intersection [b1 closed-bezier/c] [b2 closed-bezier/c]) (listof closed-bezier/c)]{
                                                                        
Intersect the second closed bezier curve with the first, produces a list of closed bezier curves.}


@subsection{Bounding Boxes}

@defthing[bounding-box/c flat-contract?]{
                                         
A Bounding Box can be a pair of @racket[vec] with the first one representing the
lower left corner and the second one the upper right corner, or @racket[#f] for the null Bounding Box.}

@defproc[(combine-bounding-boxes [bb bounding-box/c] [b bounding-box/c] ...) bounding-box/c]{
                                                                                             
Produces the bounding box of the bounding boxes.}

@defproc[(line-bounding-box [l (cons/c vec? vec?)]) bounding-box/c]{
                                                                                             
Produces the bounding box of the line segment represented by the two @racket[vec].}

@defproc[(inside-bounding-box? [v vec?] [bb bounding-box/c]) boolean?]{
                                                                                       
True if the @racket[vec] is inside the bounding box.}

@defproc[(overlap-bounding-boxes? [bb1 bounding-box/c] [bb2 bounding-box/c]) boolean?]{
                                                                                       
True if the bounding boxes overlap.}

@defproc[(include-bounding-box? [bb1 bounding-box/c] [bb2 bounding-box/c]) boolean?]{
                                                                                       
True if the second bounding box is surrounded by the first one.}

@section{Parametric fonts and font macros}

@defmodule[sfont/parametric/fontwriter]

The modules described above are useful for reading, modifying and inspecting fonts.
However, to define new fonts sfont has macros tha should be easier to use.


@subsection{Bezier curves}

Instead of writing a bezier curve point by point the macro @racket[~] is provided,
we will discuss how to use it with examples:

@defform[(~ path-element ...)]

The simplest use is equivalent to specify a cubic bezier point by point
@(define ss-eval (make-base-eval))
@(void (interaction-eval #:eval ss-eval (require sfont/parametric/fontwriter
                                                 
                                                 sfont/geometry
                                                 racket/math
                                                 sfont)))

@interaction[#:eval ss-eval
                    (~ (0 0) (55 0) (45 100) (100 100))]

A line is expressed with a double hyphen, @racket[--] (from now on the
bezier curves will be printed with @racket[print-beziers]):

@interaction[#:eval ss-eval
                    (print-beziers
                     (~ (0 0) -- (500 0) -- (500 500) -- (0 500) -- (0 0)))]

To close the curve, instead of repeating the firt point it is possible to use
the @racket[cycle] command:

@interaction[#:eval ss-eval
                    (print-beziers
                     (~ (0 0) -- (500 0) -- (500 500) -- (0 500) -- cycle))]

Coordinates can be relative to the last point:
@interaction[#:eval ss-eval
                    (print-beziers
                     (~ (0 0) -- (\@ 500 0) -- (\@ 0 500) -- (\@ -500 0) -- cycle))]

Or using an angle and a distance
@interaction[#:eval ss-eval
                    (print-beziers
                     (~ (0 0) -- (\@° 0 500) -- (\@° (* pi 2/3) 500) -- cycle))]

Curve segments can be defined in an alternative way:
@interaction[#:eval ss-eval
                    (print-beziers
                     (~ (0 0) (500 0 0.55) (500 500) -- (0 500) -- cycle))]

where control points are obtained interpolating between @racket[(0 0)] and @racket[(500 0)]
(with an interpolation factor @racket[0.55]) and between @racket[(500 500)] and @racket[(500 0)];

Finally we can @racket[insert] another bezier inside @racket[~] (the part inserted is joined to the rest with
line segments).
@interaction[#:eval ss-eval
                    (define b1 (~ (100 0) -- (100 600)))
                    (print-beziers
                     (~ (-100 0) (insert b1) cycle))]

@subsection{Glyphs and Fonts}

@defform[(glyph. name 
                 maybe-locals
                 [metrics left-form right-form]
                 [contours contour ...])]

@defform*[((font. name 
                  [alignments alignment-form ...]
                  maybe-variables
                  [glyphs glyph-form ...])
           (font. (name (args default-value) ...)
                  [alignments alignment-form ...]
                  maybe-variables
                  [glyphs glyph-form ...]))]

An example for glyphs:
@interaction[#:eval ss-eval
                    (glyph->pict
                     (glyph. 'o
                             [locals (weight 100)
                                     (width 400)
                                     (height 500)]
                             [metrics 20 20]
                             [contours
                              (~ (0 0) -- (\@ width 0) -- (\@ 0 height) -- (\@ (- width) 0) -- cycle)
                              (~ (weight weight) 
                                 -- (\@ 0 (- height (* 2 weight))) 
                                 -- (\@ (- width (* 2 weight)) 0) 
                                 -- (\@ 0 (- (* 2 weight) height))
                                 -- cycle)]))]

Fonts can be defined in two ways: the first one creates a @racket[font],
the second produces an anonymous procedure that can be called with the
arguments defined and that produces a @racket[font].

@racketblock[(define f (font. (squarefont (weight 100) (width 200))
                              ...))]

@racket[f] can be called with keyword arguments @racket[(f #:weight 120 #:width 210)].

The body of the font can then use @racket[weight] and @racket[width] to create new @racket[font]s.

The creation of parametric fonts can then be made easier since the designer decide which 'parameter' will be exposed,
while other 'hidden' variables can be defined in the @racket[maybe-variables] part.

A complex example taken from the @racket[sfont-examples] collection:

@racketblock[(define sq 
               (font. (squarefont  [x-height 500] [width 0.5] [weight 0.5]) 
                      (alignments
                       [base 0 -10]
                       [xh x-height 10]
                       [desc* (/ (- x-height 1000) 2) 0 :font-descender]
                       [asc* (- x-height (alg desc*)) 0 :font-ascender]
                       [dsc (+ (alg desc*) 10) -10]
                       [ascender (- (alg asc*) 10) 10])
                      (variables
                       [gw (* 1000 width)]
                       [v-stem (* x-height weight 0.333)]
                       [h-stem (* v-stem 0.9)]
                       [space (/ (- gw (* 2 v-stem)) 2)]
                       [x1 space]
                       ...)
                      (glyphs
                       (glyph. 'a
                               (metrics space space)
                               [contours a-cnt])
                       (glyph. 'b
                               (metrics space (/--/ (+ gw space space)))
                               [contours
                                (rect x1 y1 v-stem (alg ascender))
                                (rect x1 y1 gw x-height)
                                (reverse (rect (+ x1 v-stem) (+ y1 h-stem) 
                                               (- gw (* 2 v-stem)) (- x-height (* 2 h-stem))))])
                       ...)))]

Alignments are given in the form @racket[[name position overshoot-height]],
to get the values back the procedures @racket[alg], @racket[ovs], @racket[ovs-height],
are defined:

@defthing[alignment/c flat-contract?]

@defproc[(alg [a alignment/c]) real?]{
                                      
Produces the position of the alignment.}

@defproc[(ovs [a alignment/c]) real?]{
                                      
Produces the position of the overshoot for the given alignment.}

@defproc[(ovs-height [a alignment/c]) real?]{
                                      
Produces the height of the oveshoot for the given alignment.}

@subsection{Other functions and macros}

@defproc[(rect [x real?] [y real?] [w real?] [h real?]) cubic-bezier/c]{
                                                                            
Produces a cubic bezier path of a rectangle. @racket[x], and @racket[y]
represent the lower left corner, @racket[w] and @racket[h], the width and height.}

@defproc[(ellipse [x real?] [y real?] [w real?] [h real?]) cubic-bezier/c]{
                                                                            
Produces a cubic bezier path of an ellipse. @racket[x], and @racket[y]
represent the lower left corner, @racket[w] and @racket[h], the width and height.}

@defproc[(arc [cx real?] [cy real?] [r real?] [a real?]) cubic-bezier/c]{
                                                                            
Produces a cubic bezier path of an open arc. @racket[cx], and @racket[cy]
represent the center, @racket[a] and @racket[r], the angle and radius.}

@defproc[(remove~ [pt (and/c closed-bezier/c cubic-bezier/c)]
                  [rm (and/c closed-bezier/c cubic-bezier/c)] ...)
         (listof (and/c closed-bezier/c cubic-bezier/c))]{
                                                          
Remove the paths @racket[rm] from @racket[pt].}
                                                         
                                                         
@defproc[(join~ [pt (and/c closed-bezier/c cubic-bezier/c)]
                [pts (and/c closed-bezier/c cubic-bezier/c)] ...)
         (listof (and/c closed-bezier/c cubic-bezier/c))]{
                                                          
Add the paths @racket[pts] to @racket[pt] (remove overlaps).}

                                                         
@deftogether[(@defform*[((translate. o arg tx ty)
                         (translate. o from (x y) arg tx ty))]
               @defform*[((rotate. o arg angle)
                          (rotate. o from (x y) arg angle))]
               @defform*[((scale. o arg fx [fy fx])
                          (scale. o from (x y) arg fx [fy fx]))]
               @defform*[((skew-x. o arg angle)
                          (skew-x. o from (x y) arg angle))]
               @defform*[((skew-y. o arg angle)
                          (skew-y. o from (x y) arg angle))]
               @defform*[((reflect-x. o arg)
                          (reflect-x. o from (x y)))]
               @defform*[((reflect-y. o arg)
                          (reflect-y. o from (x y) arg))])]{
                                                            
Like the corresponding transformations, but can be used with bezier paths
and with the @racket[from] command that specifies the center of trasformation.}
                                                           
@section{Graphical interfaces}
@defmodule[sfont/windows]

@defproc[(animate [font-proc (-> real? font?)]
                  [width natural-number/c]
                  [height natural-number/c]
                  [start real? 0]
                  [end real? 100]
                  [inc-proc (-> real? real?)
                            (curry + 10)])
         font?]{
                
Creates an animation window, the @racket[font-proc] procedure
takes a number produced by the @racket[inc-proc] procedure
and produces a font; @racket[inc-proc] updates the 'counter'
value (starting from @racket[start]) and end when the @racket[end]
value is reached, returning the last font produced by @racket[font-proc].}
               
@defform[(animate-fonts f ...)]{
                                
Given some interpolabel fonts produces a new animation that interpolates between the fonts.}

@defform[(slider-application
          [font font-proc final-proc]
          [sliders (slider-name slider-min slider-max init-value) ...]
          [text txt size])]{

Produces a 'thunk' that, when called, creates a new application with sliders that control the font procedure.
The number of arguments that @racket[font-proc] accepts is equal
to the number of sliders, with the same order.
The @racket[text] form specifies a string to initialize the text field, 
and a size.
The @racket[final-proc] is a procedure that will be called before saving the font.

An example that uses the font @racket[sq] in the sfont-examples directory.

@racketblock[(define (sq-proc weight width)
               (sq #:weight (/ weight 1000) #:width (/ width 1000)))
      
             (define main
               (slider-application
                [font sq-proc identity]
                [sliders
                 (weight 0 1000 500)
                 (width 0 1000 500)]
                [text "cabde o bacco" 100]))
             
             (main)]
}
                         
@section{Utilities}

@defmodule[sfont/utilities]

@defproc[(double [n real?]) real?]{Doubles a number.}

@defproc[(° [d real?]) real?]{Convert degrees in radians.}

@deftogether[(@defthing[pi/2 real?]
               @defthing[pi/3 real?]
               @defthing[pi/4 real?]
               @defthing[pi/6 real?]
               @defthing[2pi real?])]{Common angles.}
                                     
@defproc[(string->text [str string?]) (listof (listof symbol?))]{
                                                                 
Produces a 'text' that can be used with @racket[display-text]:
                                        
@racketblock[(display-text (string->text "Straße"))]}   
                                                     
@defproc[(unicode [n name/c]) (listof natural-number/c)]{
                                                         
Produces a list of Unicode codes for a given name 
(using the @link["http://partners.adobe.com/public/developer/en/opentype/glyphlist.txt"]{Adobe Glyph List}).}

