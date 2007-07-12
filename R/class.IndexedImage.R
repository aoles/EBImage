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
  function (x, ...) {
    class (x) <- "Image"
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
setMethod ("animate", signature(x="IndexedImage"),
  function (x, ...) {
    if ( !.isCorrectType(x) ) x <- .correctType (x)
    invisible ( .DoCall("lib_animate", normalize(x) ) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("tile", signature(x="list"),
    function (x, nx=10, lwd=1, fg.col="#E4AF2B", bg.col="black", ...) {
        lapply(x, tile, nx=nx, lwd=lwd, fg.col=fg.col, bg.col=bg.col, ...)
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
setMethod ("features", signature (x="IndexedImage"),
  function (x, ...) {
    if ( length(x@features) > 0 ) 
      return( x@features )
    else
      return( getFeatures(x)@features )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("getFeatures", signature(x="IndexedImage"),
  function (x, ref, N = 12, R = 30, apply.Gaussian=TRUE, nc = 256, ...) {
    if ( !missing(ref) && is.Image(ref) && !(colorMode(ref) == Grayscale) ) 
      .stop( "if present, 'ref' must be Grayscale" )
    .dim <- dim(x)
    hf <- hull.features( x )
    if ( !missing(ref) ) {
      ef <- edge.features( x=x, ref=ref )
      tf <- haralick.features(x=x, ref=ref, nc=nc)
      zf <- zernike.moments(x=x, ref=ref, N=N, R=R, apply.Gaussian=apply.Gaussian)
      ## mf calculation
      mf <- moments(x=x, ref=ref)
      ## distance from COM to geometric centre
      if ( .dim[3] == 1 ) 
        mf <- cbind(mf, sqrt((mf[,3,drop=FALSE]-hf[,1])^2 +(mf[,4]-hf[,2])^2))
      else {
        for ( i in seq_along(hf) ) 
          mf[[i]] <- cbind(mf[[i]], sqrt((mf[[i]][,3,drop=FALSE]-hf[[i]][,1])^2 +
                                                     (mf[[i]][,4]-hf[[i]][,2])^2))
      }
      do.moms <- function(m) {
        m <- cbind(m[,2,drop=FALSE], m[,2]/m[,1], m[,18], 2*sqrt(m[,9]), 
                   2*sqrt(m[,10]), sqrt((m[,9] - m[,10])/m[,9]), m[,11:17,drop=FALSE])
        m[ which(is.na(m)) ] = 0.0
        colnames(m) <- c("i.int", "i.dens", "i.d", "i.s2maj", "i.s2min", "i.ecc", 
                         "i.I1", "i.I2", "i.I3", "i.I4", "i.I5", "i.I6", "i.I7")
        m
      }
      if ( .dim[3] == 1 ) mf <- do.moms(mf)
      else mf <- lapply(mf, do.moms)
    }
    else { # missing ref
      ef <- edge.features( x=x )
    }

    if ( .dim[3] == 1 ) {
      if ( !missing(ref) )
        x@features <- list( cbind(hf, ef, tf, mf, zf) )
      else
        x@features <- list( cbind(hf, ef) )
    }
    else {
      x@features <- vector("list", length(hf))
      if ( !missing(ref) )
        for ( i in seq_along(hf) ) x@features[[i]] <- cbind(hf[[i]], ef[[i]], tf[[i]], mf[[i]], zf[[i]])
      else
        for ( i in seq_along(hf) ) x@features[[i]] <- cbind(hf[[i]], ef[[i]])
    }
    return( x )
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
  function (x, ref, rotate=TRUE, bg.col="black", ...) {
    if ( colorMode(x) != Grayscale )
      .stop( "'x' must be Grayscale" )
    if ( any(dim(x) != dim(ref)) )
      .stop( "dim(x) must equal dim(ref)" )
    .dim <- dim(x)
      
    ## determine the bounding box -- same for all images in stack
    hf <- hull.features( x )
    if ( .dim[3] == 1 ) {
      xyt <- hf[,c(1,2,11)]
      ext <- hf[,12] ## h.s2major ~ h.pdm + h.pdsd
    }
    else {
      xyt <- lapply( hf, function(x) x[,c(1,2,11)] )
      ext <- unlist( lapply(hf, function(x) x[,12]) ) ## h.s2major ~ h.pdm + h.pdsd
    }
    ext <- quantile(ext, 0.95, na.rm=TRUE)
    if ( colorMode(ref) == TrueColor ) col <- channel(bg.col, "rgb")
    else col <- channel(bg.col, "gray")
    ## create image headers for the result: better to do in C, but too complicated
    ## this hdr will be copied in C code, not modified
    if ( .dim[3] == 1 ) {
      hdr <- header(ref)
      hdr@.Data <- array(col, c(1,1,1))
    }
    else {
      hdr <- vector("list", .dim[3])
      for ( i in 1:.dim[3] ) {
        hdr[[i]] <- header(ref)
        hdr[[i]]@.Data <- array(col, c(1,1,1))
      }
    }
    .DoCall ("lib_stack_objects", x, ref, hdr, xyt, as.numeric(ext), as.integer(rotate))
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

