#lang racket

(struct ufo:font 
  (format creator fontinfo groups kerning features layers lib)
  #:transparent)

(struct ufo:group
  (name glyphs)
  #:transparent)

  
  