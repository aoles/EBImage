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
setMethod ("paintObjects", signature(x="ImageX", tgt="ImageX"),
  function (x, tgt, opac=c(0.4, 0.05, 0.4), col=c("#FFC72C","#5BABF6","#FF372C")) {
    if ( colorMode(x) == TrueColor ) stop("'x' must be an Image not in \'TrueColor\' color mode")
    
    if ( any( dim(x)[1:2] != dim(tgt)[1:2] ) )
      .stop( "'x' and 'tgt' must have the same size" )
    
    if (getNumberOfFrames(x,'render') != getNumberOfFrames(tgt,'render'))
      .stop( "'x' and 'tgt' must have the same number of render frames" )                           
      
    if ( length(opac) < 3 || length(col) < 3 )
      .stop( "'opac' and 'col' must have at least 3 elements each: opacity and color of the edge, of the background, of the object contact" )
    opac <- as.numeric (opac)
    if ( any(opac < 0) || any(opac > 1) )
      .stop("all opacity values must be in the range [0,1]" )
    col <- as.character (col)
    return ( .ImageCall("paintObjects", x, tgt, opac, col) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("stackObjects", signature(x="ImageX", ref="ImageX", index="missing"),
  function (x, ref, index, combine, rotate, bg.col, ext, centerby, rotateby) {
    if ( colorMode(x) == TrueColor ) stop("'x' must be an Image not in \'TrueColor\' color mode")
    dimx <- dim(x)
    if ( any(dimx != dim(ref)) )
      stop( "dim(x) must equal dim(ref)" )

    if (missing(rotate)) rotate = TRUE
    if (missing(combine)) combine = TRUE
    
    # get centres
    if (colorMode(ref)==TrueColor && (!missing(centerby)||!missing(rotateby)))
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
    res <- .ImageCall ("stackObjects", x, ref, hdr, xyt, as.numeric(ext), as.integer(rotate))
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
setMethod ("stackObjects", signature(x="ImageX", ref="ImageX", index="character"),
  function (x, ref, index) {
    index <- strsplit(index, ".", fixed=TRUE)
    fct <- unlist(lapply(index, function(x) x[1]))
    index <- split( as.numeric(unlist(lapply(index, function(x) x[2]))), fct )
    if ( dim(x)[3] == 1 )
      return( stackObjects(x, ref, index=as.numeric(index)) )
    stackObjects(x, ref, index=index)
  }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("stackObjects", signature(x="ImageX", ref="ImageX", index="list"),
  function (x, ref, index, combine) {
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
    s <- stackObjects(x, ref, combine=FALSE)
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
setMethod ("stackObjects", signature(x="ImageX", ref="ImageX", index="numeric"),
  function (x, ref, index) {
    .dim <- dim(x)
    if ( .dim[3] > 1 )
      stop("index cannot be numeric if there is more than 1 frame in 'x', consider character or list-type index")
    s <- stackObjects(x, ref)
    if ( sum(index > dim(s)[3]) > 0 ) index <- index[ index <= dim(s)[3] ]
    return( s[,,index] )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("rmObjects", signature(x="ImageX", index="list"),
  function (x, index) {
    if ( colorMode(x) == TrueColor ) stop("'x' must be an Image not in \'TrueColor\' color mode")
    index <- lapply (index, as.integer)
    return ( .ImageCall ("rmObjects", x, index ) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("rmObjects", signature(x="ImageX", index="numeric"),
  function (x, index) {
    if ( colorMode(x) == TrueColor ) stop("'x' must be an Image not in \'TrueColor\' color mode")
    index <- list( as.integer(index) )
    return ( .ImageCall ("rmObjects", x, index ) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("reenumerate", signature(x="ImageX"),
  function(x) {
    if (any(max(x)<0)) stop("'x' contains negative values and is incorrectly formed")
    validObject(x)
    .dim=dim(x)
    
    storage.mode(x)='integer'
    dim(x)=c(.dim[1:2],getNumberOfFrames(x,'total'))

    imageData(x) = apply(imageData(x), 3, function(im) {
      from = as.integer(names(table(im)))
      to = seq_along(from)-1
      to[match(im, from)]
    })
    dim(x)=.dim
    storage.mode(x)="numeric"
    
    x
  }
)
