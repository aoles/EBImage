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
setClass ( "IndexedImage", contains="Image" )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("as.Image", signature(x="IndexedImage"),
  function (x, ...) {
    class (x, "Image")
    return( x )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
print.IndexedImage <- function(x, ...) show(x)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("display", signature(x="IndexedImage"),
  function (x, no.GTK=FALSE, ...) {
    if ( !.isCorrectType(x) )
      x <- .correctType ( normalize(x))
    else
      x <- normalize (x)
    invisible ( .DoCall("lib_display", x, as.logical(no.GTK) ) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("watershed", signature(x="Image"),
  function (x, tolerance=1, ext=1, ...) {
    if ( colorMode(x) != Grayscale )
       .stop( "'x' must be Grayscale" )
    tolerance <- as.numeric (tolerance)
    if ( tolerance < 0 )
      .stop( "'tolerance' must be non-negative" )
    ext <- as.integer (ext)
    if ( ext < 1 )
      .stop( "'ext' must be a positive integer" )
    return( .DoCall("lib_filterInvWS", x, tolerance, ext) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("propagate", signature(x="Image", seeds="IndexedImage"),
  function (x, seeds, mask=NULL, lambda=0.005, ext=1, ...) {
    if ( colorMode(x) != Grayscale )
      .stop("'x' must be Grayscale" )
    if ( !assert(x, seeds, strict=TRUE) || (!is.null(mask) && !assert(x, mask, strict=TRUE)) )
      .stop( "dim(x) must equal dim(seeds) and dim(mask) if mask is not NULL, all images must be Grayscale" )
    ext <- as.integer (ext)    
    if ( ext < 1 )
      .stop("'ext' must be a positive integer" )
    lambda <- as.numeric (lambda)
    if ( lambda < 0.0 )
      .stop("'lambda' must be non-negative" )
    return( .DoCall( "lib_propagate", x, seeds, mask, ext, lambda) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("getObjects", signature(x="IndexedImage", ref="Image"),
  function (x, ref, ...) {
    if ( colorMode(x) != Grayscale )
      .stop( "'x' must be Grayscale" )
    if ( !assert(x, ref, strict=TRUE) )
      .stop( "dim(x) must equal dim(ref), 'ref' must be Grayscale" )
    res <- .DoCall ("lib_getFeatures", x, ref)
    if ( is.list(res) ) {
      ## set colnames for features
      for ( i in 1:length(res) )
        if ( is.matrix( res[[i]] ) )
          if ( ncol(res[[i]] ) == 11 )
            colnames( res[[i]] ) <- c("x", "y", "size", "per", "image.border", "effr", 
                       "intensity", "acirc", "per.mean", "per.sd", "per.by.2.pi.effr")
    }
    return (res)
  }
)

setMethod ("getObjects", signature(x="IndexedImage", ref="missing"),
  function (x, ref, ...) {
    if ( colorMode(x) != Grayscale )
      .stop( "'x' must be Grayscale" )
    res <- .DoCall ("lib_getFeatures", x, NULL)
    if ( is.list(res) ) {
      ## set colnames for features
      for ( i in 1:length(res) )
        if ( is.matrix( res[[i]] ) )
          if ( ncol(res[[i]] ) == 11 )
            colnames( res[[i]] ) <- c("x", "y", "size", "per", "image.border", "effr",
                       "intensity", "acirc", "per.mean", "per.sd", "per.by.2.pi.effr")
    }
    return (res)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("paintObjects", signature(x="IndexedImage", tgt="Image"),
  function (x, tgt, opac=c(0.4, 0.05, 0.4), col=c("#FFC72C","#5BABF6","#FF372C"), ...) {
    if ( colorMode(x) != Grayscale )
      .stop( "'x' must be Grayscale" )
    if ( any( dim(x) != dim(tgt) ) )
      .stop( "dim(x) must equal dim(tgt)" )
    if ( length(opac) < 3 || length(col) < 3 )
      .stop( "'opac' and 'col' must have at least 3 elements each: opacity and color of the edge, of the background, of the object contact" )
    opac <- as.numeric (opac)
    if ( any(opac < 0) || any(opac > 1) )
      .stop("all opacity values must be in the range [0,1]" )
    col <- as.character (col)
    return ( .DoCall("lib_paintFeatures", x, tgt, opac, col) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("matchObjects", signature(x="IndexedImage", ref="IndexedImage"),
  function (x, ref, ...) {
    if ( colorMode(x) != Grayscale )
      .stop( "'x' must be Grayscale" )
    if ( !assert(x, ref, strict=TRUE) )
      .stop( "dim(x) must equal dim(ref), 'ref' must be Grayscale" )
    return ( .DoCall ("lib_matchFeatures", x, ref) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("stackObjects", signature(x="IndexedImage", ref="Image"),
  function (x, ref, ...) {
    if ( colorMode(x) != Grayscale )
      .stop( "'x' must be Grayscale" )
    if ( any(dim(x) != dim(ref)) )
      .stop( "dim(x) must equal dim(ref)" )
    return ( .DoCall ("lib_stackFeatures", x, ref) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("rmObjects", signature(x="IndexedImage", index="list"),
  function (x, index, ...) {
    if ( colorMode(x) != Grayscale )
      .stop( "'x' must be Grayscale" )
    index <- lapply (index, as.integer)
    return ( .DoCall ("lib_deleteFeatures", x, index ) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("rmObjects", signature(x="IndexedImage", index="numeric"),
  function (x, index, ...) {
    if ( colorMode(x) != Grayscale )
      .stop( "'x' must be Grayscale" )
    index <- list( as.integer(index) )
    return ( .DoCall ("lib_deleteFeatures", x, index ) )
  }
)

