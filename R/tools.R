# Package tools

# Copyright (c) 2006 Oleg Sklyar

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
.DoCall <- function (name, ...) {
    .Call(name, ..., PACKAGE = "EBImage")
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
. <- function (string) {
  ##    .DoCall ("lib_", as.character(string) )
  return (string)
}

.stop <- function (string, ...) stop( .(string), ... )
.warning <- function (string, ...) warning( .(string), ... )
.cat <- function (string, ...) cat( .(string), ... )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("channel", signature(x="ANY", mode="character"),
  function (x, mode, ...) {
    mode <- tolower (mode)
    modeNo <- as.integer( switch (EXPR=mode, rgb=0, grey=, gray=1, r=, red=2, 
            g=, green=3, b=, blue=4, asred=5, asgreen=6, asblue=7, x11=8, -1) )
    if ( modeNo < 0 )
      stop( "wrong conversion mode")
    if ( !is.numeric(x) && !is.character(x) )
      stop( "argument must be coercible to either numeric or character" )
    res <- .DoCall("lib_channel", x, modeNo )
    if ( !is.null(res) )
      res [ which( is.na(x) ) ] = NA
    if ( is.null(res) || is.character(res) ) return (res)
    if ( is.array(x) ) dim (res) <- dim (x)
    return (res)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("floodFill", signature(x="array", pt="ANY"),
  function(x, pt, col, tolerance=1e-3, ...) {
    .dim = dim(x)
    if (length(.dim)<2)
      stop("'x' must have (at least) 2 dimensions")
    pt = as.integer(pt)
    if (length(pt)<2)
      stop("'pt' must contain at least 2 values for x and y coordinates")
    if (any(pt)<1 || any(pt[1:2]>.dim[1:2]))
      stop("coordinates of the start point must be inside the image boundaries")
    tolerance = as.numeric(tolerance)
    if (missing(col)) col=x[pt[1]+pt[2]*.dim[1]]
    # allows for conversion from X11 color string to its RGB or grayscale value
    if (is.character(col))
      col <- if (is.integer(x)) channel(col,"rgb") else channel(col,"gray")
    else
      col <- if (is.integer(x)) as.integer(col) else col=as.double(col)
    return( .DoCall("lib_floodFill", x, pt, col, tolerance))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rgbImage = function(r,g,b) {
  res = list()
  if (!missing(r)) res$r = channel(r,"asred")
  if (!missing(g)) res$g = channel(g,"asgreen")
  if (!missing(b)) res$b = channel(b,"asblue")
  switch(length(res),
    res[[1]],
    res[[1]]+res[[2]],
    res[[1]]+res[[2]]+res[[3]]
  )
}

