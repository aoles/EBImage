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
setMethod ("tile", signature(x="list"),
    function (x, nx=10, lwd=1, fg.col="#E4AF2B", bg.col="black", ...) {
        lapply(x, tile, nx=nx, lwd=lwd, fg.col=fg.col, bg.col=bg.col, ...)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("fillHull", signature(x="IndexedImage"),
  function(x, ...) {
    return(.DoCall("lib_fillHull", x))
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
  function (x, seeds, mask=NULL, lambda=0.1, ext=1, ...) {
    if ( colorMode(x) != Grayscale )
      .stop("'x' must be Grayscale" )
    if ( !assert(x, seeds, strict=TRUE) || (!is.null(mask) && !assert(x, mask, strict=TRUE)) )
      .stop( "dim(x) must equal dim(seeds) and dim(mask) if mask is not NULL, all images must be Grayscale" )
    ext <- as.integer (ext)
    if ( ext < 0 )
      .stop("'ext' must be non-negative" )
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
  function (x, ref, N = 12, R = 30, apply.Gaussian=TRUE, nc = 256, pseudo=FALSE, ...) {
    if ( !missing(ref) && is.Image(ref) && !(colorMode(ref) == Grayscale) )
      .stop( "if present, 'ref' must be Grayscale" )
    .dim <- dim(x)
    hf <- hullFeatures( x )
    if ( !missing(ref) ) {
      ef <- edgeFeatures( x=x, ref=ref )
      tf <- haralickFeatures(x=x, ref=ref, nc=nc)
      zf <- zernikeMoments(x=x, ref=ref, N=N, R=R, apply.Gaussian=apply.Gaussian, pseudo=pseudo)
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
setMethod ("stackObjects", signature(x="IndexedImage", ref="Image", index="missing"),
  function (x, ref, index, combine, rotate, bg.col, ext, centerby, rotateby, ...) {
    if ( colorMode(x) != Grayscale )
      stop( "'x' must be Grayscale" )
    dimx <- dim(x)
    if ( any(dimx != dim(ref)) )
      stop( "dim(x) must equal dim(ref)" )

    if (missing(rotate)) rotate = TRUE
    if (missing(combine)) combine = TRUE
    
    # get centres
    if (colorMode(ref)==Grayscale && (!missing(centerby)||!missing(rotateby)))
      warning("'centerby' and 'rotateby' are only meaningful for TrueColor images")
    if (missing(centerby)) centerby = "gray"
    if (missing(rotateby)) rotateby = "gray"

    centerby = tolower(centerby)
    rotateby = tolower(rotateby)
    if (!all(centerby%in%c("gray","grey","red","green","blue")) ||
        !all(rotateby%in%c("gray","grey","red","green","blue")))
      stop("'centerby' and 'rotateby' must be any one of 'gray', 'grey', 'red', 'green', 'blue'")

    # for grayscale images default conversion to gray will not do anything
    # get centers (using cmoments) and theta using moments
    if (centerby==rotateby || !rotate) {
      # use 'moments' to get both centers and theta
      xyt = moments(x, channel(ref, centerby))
      if (dimx[3]==1) xyt = list(xyt)
      if (missing(ext)) extx = unlist(sapply(xyt, function(m) m[,9])) #  l1: 2*sqrt(l1) ~ h.pdm + h.pdsd
      xyt = lapply(xyt, function(m) m[,c(3,4,8),drop=FALSE])
    } else {
      # use cmoments to get xy
      xy = cmoments(x, channel(ref, centerby))
      if (dimx[3]==1) xy = list(xy)
      # use moments to get theta
      xyt = moments(x, channel(ref, rotateby))
      if (dimx[3]==1) xyt = list(xyt)
      if (missing(ext)) extx = unlist(sapply(xyt, function(m) m[,9])) #  l1: 2*sqrt(l1) ~ h.pdm + h.pdsd
      xyt = mapply(function(coord,theta) cbind(coord[,c(3,4),drop=FALSE],theta[,8]),
                   xy, xyt, SIMPLIFY=FALSE, USE.NAMES=FALSE)
    }
    if (missing(ext)) ext = 2.0*sqrt(quantile(extx,0.98,na.rm=TRUE)) # 2*sqrt(l1) ~ h.pdm + h.pdsd

    if ( missing(bg.col) ) bg.col <- "black"
    if ( colorMode(ref) == TrueColor ) col <- channel(bg.col, "rgb")
    else col <- channel(bg.col, "gray")

    ## create image headers for the result: better to do in C, but too complicated
    ## this hdr will be copied in C code, not modified
    hdr <- header(ref)
    hdr@.Data <- array(col, c(1,1,1))
    if (dimx[3]==1) xyt = xyt[[1]]
    res <- .Call ("lib_stack_objects", x, ref, hdr, xyt, as.numeric(ext), as.integer(rotate))
    if (!combine || !is.list(res)) return( res )
#    ## if we are here, we have more than one frame and hf is a list
#    ## index of frames with no objects, these are to remove
#    index <- which( unlist( lapply(hf, function(x) all(x[,"h.s"] == 0)) ) )
#    ## if all are empty we return the first empty one
#    if ( length(index) == .dim[3] ) return( res[[1]] )
#    if ( length(index) > 0 ) res <- res[ -index ]
    return( combine(res) )
  }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("stackObjects", signature(x="IndexedImage", ref="Image", index="character"),
  function (x, ref, index, ...) {
    index <- strsplit(index, ".", fixed=TRUE)
    fct <- unlist(lapply(index, function(x) x[1]))
    index <- split( as.numeric(unlist(lapply(index, function(x) x[2]))), fct )
    if ( dim(x)[3] == 1 )
      return( stackObjects(x, ref, index=as.numeric(index), ...) )
    stackObjects(x, ref, index=index, ...)
  }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("stackObjects", signature(x="IndexedImage", ref="Image", index="list"),
  function (x, ref, index, combine, ...) {
    if ( missing(combine) )
      combine <- FALSE
    .dim <- dim(x)
    if ( !is.null(names(index)) ) {
      suppressWarnings( ims <- as.numeric(names(index)) )
      if ( any(is.na(ims)) | any(ims > .dim[3]) | any(ims < 1) )
        stop("if 'index' is a named list, names must correspond to frame indexes")
    } else {
      if ( length(index) != .dim[3] )
        stop("if 'index' is an unnamed list, its length must be equal to the number of frames")
      names(index) <- 1:(.dim[3])
    }
    s <- stackObjects(x, ref, combine=FALSE, ...)
    if ( .dim[3] == 1 ) s <- list(s)
    res <- list()
    for ( i in as.numeric(names(index)) ) {
      j <- index[[as.character(i)]]
      if ( sum(j > dim(s[[i]])[3]) > 0 ) j <- j[ j <= dim(s[[i]])[3] ]
      res[[as.character(i)]] <- s[[i]][,,j]
    }
    if ( length(res) < 2 || !combine ) return( res )
    return( combine(res) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("stackObjects", signature(x="IndexedImage", ref="Image", index="numeric"),
  function (x, ref, index, ...) {
    .dim <- dim(x)
    if ( .dim[3] > 1 )
      stop("index cannot be numeric if there is more than 1 frame in 'x', consider character or list-type index")
    s <- stackObjects(x, ref, ...)
    if ( sum(index > dim(s)[3]) > 0 ) index <- index[ index <= dim(s)[3] ]
    return( s[,,index] )
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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("reenumerate", signature(x="IndexedImage"),
  function(x, ...) {
    if (any(max(x)<0))
      stop("'x' contains negative values and is incorrectly formed")
    mode(x) = "integer"
    y = apply(imageData(x), 3, function(im) {
      from = as.integer(names(table(im)))
      to = seq_along(from)-1
      to[match(im, from)]
    })
    mode(y) = "double"
    dim(y) = dim(x)
    x = header(x)
    x@.Data = y
    x
  }
)
