# IndexedImage class definitions and methods

# Copyright (c) 2005-2007 Oleg Sklyar

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
setClass ("IndexedImage", contains="Image")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("as.Image", signature(x="IndexedImage"),
  function (x) {
    class (x) <- "Image"
    return( x )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
print.IndexedImage <- function(x, ...) show(x)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("tile", signature(x="list"),
    function (x, nx=10, lwd=1, fg.col="#E4AF2B", bg.col="black", ...) {
        lapply(x, tile, nx=nx, lwd=lwd, fg.col=fg.col, bg.col=bg.col, ...)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("fillHull", signature(x="IndexedImage"),
  function(x, ...) {
    return(.DoCall("fillHull", x))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("watershed", signature(x="Image"),
  function (x, tolerance=1, ext=1, ...) {
    if ( colorMode(x) == TrueColor ) stop("'x' must be an Image not in \'TrueColor\' color mode")
    tolerance <- as.numeric (tolerance)
    if ( tolerance < 0 )
      .stop( "'tolerance' must be non-negative" )
    ext <- as.integer (ext)
    if ( ext < 1 )
      .stop( "'ext' must be a positive integer" )
    return( .DoCall("watershed", x, tolerance, ext) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("propagate", signature(x="Image", seeds="IndexedImage"),
  function (x, seeds, mask=NULL, lambda=0.1, ext=1, seed.centers=FALSE, ...) {
    if ( colorMode(x) == TrueColor ) stop("'x' must be an Image not in \'TrueColor\' color mode")
    if ( !assert(x, seeds, strict=TRUE) || (!is.null(mask) && !assert(x, mask, strict=TRUE)) )
      .stop( "dim(x) must equal dim(seeds) and dim(mask) if mask is not NULL, all images must be Grayscale" )
    ext <- as.integer (ext)
    if ( ext < 0 )
      .stop("'ext' must be non-negative" )
    lambda <- as.numeric (lambda)
    if ( lambda < 0.0 )
      .stop("'lambda' must be non-negative" )
    if (seed.centers) {
      cm = cmoments(seeds)
      dimx = dim(seeds)
      if (dimx[3]==1) cm = list(cm)
      index = lapply(cm, function(xy) {
        floor(xy[,3]) + floor(xy[,4])*dimx[1]
      })
      for (i in 1:dimx[3]) index[[i]] = index[[i]] + (i-1)*dimx[1]*dimx[2]
      index = unlist(index)
      s = seeds
      s[] = 0.0
      s[index] = seeds[index]
      seeds = s
    }
    return( .Call( "lib_propagate", x, seeds, mask, ext, lambda) )
  }
)
