# Drawables for class Image

# Copyright (c) 2007 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
drawfont <- function(family="Arial", style="n", size=14, weight=200, antialias=TRUE) {
  res <- list(family=family, style=style, size=size, weight=weight, antialias=antialias)
  class(res) <- "DrawFont"
  res
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("drawtext", signature(img="Image", xy="numeric", labels="character"),
  function(img, xy, labels, font, col, ...) {
    if ( missing(font) ) font <- drawfont()
    if ( missing(col) ) col <- "white"
    drawtext(img, list(as.numeric(xy)), list(labels), font, col, ...)
  }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("drawtext", signature(img="Image", xy="matrix", labels="character"),
  function(img, xy, labels, font, col, ...) {
    if ( missing(font) ) font <- drawfont()
    if ( missing(col) ) col <- "white"
    drawtext(img, list(as.numeric(xy)), list(labels), font, col, ...)
  }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("drawtext", signature(img="Image", xy="list", labels="list"),
  function(img, xy, labels, font, col, ...) {
    if (length(xy) != length(labels) || length(xy) != dim(img)[3])
      stop("lists of coordinates 'xy' labels 'labels' must be of the same length as the number of frames")
    xy <- lapply(xy, as.numeric)
    for ( i in seq_along(labels)) 
      if (!is.character(labels[[i]]))
        stop("all elements of 'labels' must be of class 'character'")
    if ( !is(font, "DrawFont") )
      stop("to set the font use the 'drawfont' function which returns an S3 class 'DrawFont', modify the slots as needed")
    if ( missing(font) ) font <- drawfont()
    if ( missing(col) ) col <- "white"
    font$style <- as.integer(switch(tolower(substr(font$style,1,1)), i=1, o=2, 0))
    font$size <- as.numeric(font$size)
    font$weight <- as.numeric(font$weight)
    font$antialias <- as.logical(font$antialias)
    .DoCall("lib_drawText", img, xy, labels, font, col)
  }
)
  

