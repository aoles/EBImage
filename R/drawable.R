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

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
drawfont = function(family=switch(.Platform$OS.type, windows="Arial", "helvetica"),
  style="n", size=14, weight=200, antialias=TRUE) {
  res <- list(family=family, style=style, size=size, weight=weight, antialias=antialias)
  class(res) <- "DrawFont"
  res
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
drawtext = function(img, xy, labels, font, col) {   
  validImage(img)
  if (is.numeric(xy)) {
    xy=list(as.numeric(xy))
    labels=list(labels)
  }
  if ( missing(font) ) font=drawfont()
  if ( missing(col) ) col="white"
  
  ## Mac stop: imageMagick/MagickWand/drawtext doesn't understand MacOS fonts
  if ( length(grep("apple", Sys.getenv("R_PLATFORM")))>0) {
    warning('drawtext doesn\'t work on Mac OS due to ImageMagick/Mac OS fonts incompatibility')
    return(img)
  }
    
  if (length(xy) != length(labels) || length(xy) != getNumberOfFrames(img,'render'))
    stop("lists of coordinates 'xy' labels 'labels' must be of the same length as the number of render frames")
  xy <- lapply(xy, as.numeric)
  for ( i in seq_along(labels)) 
    if (!is.character(labels[[i]]))
      stop("all elements of 'labels' must be of class 'character'")
  if ( !is(font, "DrawFont") )
      stop("to set the font use the 'drawfont' function which returns an S3 class 'DrawFont', modify the slots as needed")

  font$style <- as.integer(switch(tolower(substr(font$style,1,1)), i=1, o=2, 0))
  font$size <- as.numeric(font$size)
  font$weight <- as.numeric(font$weight)
  font$antialias <- as.logical(font$antialias)
  return(.Call("lib_drawText", castImage(img), xy, labels, font, col, PACKAGE='EBImage'))
}
  

