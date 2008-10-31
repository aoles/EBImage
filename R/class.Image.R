# Class Image, definition and methods

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
Grayscale <- as.integer (0)
TrueColor  <- as.integer (1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass ("Image",
  representation (
    colormode    = "integer"    ## 0 - gray, 1 - RGB etc
  ),
  prototype (
    colormode    = Grayscale
  ),
  contains = "array"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Image <- function(data=array(0.0,c(0,0,1)), dim=base::dim(data), colormode, ...) {
  ld = length(dim)
  if(!(ld%in%(2:3)))
    stop(sprintf("length(dim) must be 2 or 3, is %d.", ld))
  if (ld==2L)
    dim=c(dim, 1L)
  if (missing(colormode)) colormode = Grayscale
  res = new("Image", colormode=colormode, ...)
  res@.Data = array(
    if(colormode==TrueColor) as.integer(data) else as.double(data),
    dim=dim)
  return( res )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("as.Image", signature(x="array"),
  function (x, ...) {
    if (is.integer(x)) return(Image(x, colormode=TrueColor))
    r = range(x)
    x = Image(x, colormode=Grayscale)
    if (diff(r) > 1 || any(r) < 0 || any(r) > 2) class(x) = "IndexedImage"
    return(x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("colorMode", signature (x="Image"),
  function (x, ...) x@colormode
)
setReplaceMethod ("colorMode", signature (x="Image", value="numeric"),
  function (x, ..., value) {
    if (value == x@colormode) return (x)
    if (value == TrueColor) return ( channel(x, "RGB") )
    if (value == Grayscale) return ( channel(x, "gray") )
    # on any other unsupported value
    return(x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("imageData", signature (x="Image"),
  function (x, ...) x@.Data
)
setReplaceMethod ("imageData", signature (x="Image", value="matrix"),
  function (x, ..., value) {
    dim(value) = c(dim(value), 1)
    x@.Data = value
    if (is.integer(value)) x@colormode = TrueColor
    else x@colormode = Grayscale
    return (x)
  }
)
setReplaceMethod ("imageData", signature (x="Image", value="array"),
  function (x, ..., value) {
    diml <- length ( dim(value) )
    if ( diml < 2 || diml > 3 )
      stop( "supplied array must be 2 or 3 dimensional" )
    if (diml == 2 ) dim (value) <- c( dim(value), 1)
    x@.Data = value
    if (is.integer(value)) x@colormode = TrueColor
    else x@colormode = Grayscale
    return (x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is.Image <- function (x) {
  if (!is(x, "Image")) return(FALSE)

  if (length(dim(x)) != 3) {
    warning("incorrectly formed object, too few dimensions")
    return(FALSE)
  }
  return (TRUE)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("assert", signature (x="Image", y="Image"),
  function (x, y, strict=FALSE, ...) {
    n <- 2
    if ( !missing(strict) && strict ) n <- 3
    if ( any( dim(x)[1:n] != dim(y)[1:n] ) || colorMode(x) != colorMode(y) )
      return( FALSE )
    return( TRUE )
  }
)
setMethod ("assert", signature (x="Image", y="missing"),
  function (x, y, strict=FALSE, ...) is.Image (x)
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stopIfNotImage <- function (x) {
  if ( !is(x, "Image") )
    stop( "argument must be of class 'Image'" )
  if ( length( dim(x) ) != 3)
    stop( "array dimensions for the object of class 'Image' must be 3" )
  invisible (NULL)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("header", signature(x="Image"),
  function (x, ...) {
    x = new(class(x), colormode=colorMode(x))
    x@.Data = if (x@colormode == TrueColor) array(as.integer(0), c(0,0,1)) 
              else array(0.0,c(0,0,1))
    return(x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("copy", signature (x="Image"),
  function (x, ...) {
    x = header(x)
    if (x@colormode == TrueColor )
      x@.Data = array(as.integer(x@.Data), dim(x@.Data))
    else
      x@.Data = array(as.double(x@.Data), dim(x@.Data))
    return(x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod (".isCorrectType", signature(x="Image"),
  function (x) {
    if ( x@colormode == TrueColor && !is.integer(x) ) {
      warning( "RGB image data stored as numeric" )
      return (FALSE)
    }
    if ( x@colormode == Grayscale && !is.double(x)) {
      warning( "grayscale image data stored as integer" )
      return (FALSE)
    }
    return (TRUE)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod (".correctType", signature(x="Image"),
  function(x) {
    if (is(x, "IndexedImage")) x@colormode = Grayscale
    if (x@colormode == TrueColor) 
      mode(x@.Data) = "integer"
    else 
      mode(x@.Data) = "double"
#      x@.Data = array (as.integer(x@.Data), dim(x@.Data) )
#    else if ( x@colormode == Grayscale && !is.double(x) )
#      x@.Data = array (as.double(x@.Data), dim(x@.Data) )
    return (x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("Arith", signature(e1="Image", e2="Image"),
	function(e1, e2) {
		imageData(e1) <- callGeneric(imageData(e1), imageData(e2))
		return( e1 )
	}
)
setMethod("Arith", signature(e1="Image", e2="array"),
	function(e1, e2) {
		imageData(e1) <- callGeneric(imageData(e1), e2)
		return( e1 )
	}
)
setMethod("Arith", signature(e1="array", e2="Image"),
	function(e1, e2) {
		imageData(e2) <- callGeneric(e1, imageData(e2))
		return( e2 )
	}
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("writeImage", signature(x="Image"),
  function (x, files, quality, ...) {
    if (is(x, "IndexedImage"))
      stop("cannot write images of class IndexedImage due to data range problems. Convert the object to Image and normalize it first, or save as an R object")
    if ( missing(quality) ) quality <- 95
    if ( quality < 1 || quality > 100 )
      stop( "quality value is given in % between 1 and 100" )
    if ( missing(files) ) stop('\'files\' must be specified')
    if ( !.isCorrectType(x) ) x <- .correctType(x)
    invisible ( .DoCall("lib_writeImages", x, as.character(files), as.integer(quality) ) )
  }
)

setMethod ("write.image", signature(x="Image"),
  function (x, ...) {
    .Deprecated("writeImage", "EBImage")
    writeImage(x, ...)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
readImage <- function(files, colormode=Grayscale, ...) {
  if ( missing(files) )
    stop("argument 'files' must be present in calls to 'readImage'")
  .DoCall ("lib_readImages", as.character(files), as.integer(colormode) )
}

read.image <- function(files, colormode=Grayscale, ...) {
  .Deprecated("readImage", "EBImage")
  readImage(files, colormode, ...)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
chooseImage <- function(colormode=Grayscale) {
  .DoCall ("lib_chooseImages", as.integer(colormode))
}

choose.image <- function(colormode=Grayscale) {
  .Deprecated("chooseImage", "EBImage")
  chooseImage(colormode)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("[", signature(x="Image", i="missing", j="missing"),
  function (x, i, j, k, ..., drop) {
    if ( missing(k) ) return (x)
    tmp <- x@.Data[ , , k, drop=FALSE]
    if ( !is.array(tmp) ) return (tmp)
    res <- header (x)
    imageData (res) <- tmp
    return (res)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("[", signature(x="Image", i="numeric", j="missing"),
  function (x, i, j, ..., drop) {
    n <- nargs()
    if ( n == 2 ) return( x@.Data[i] )
    if ( n != 4 ) stop( "incorrect number of dimensions" )
    res <- header(x)
    imageData (res) <- x@.Data[i,,...,drop=FALSE]
    return( res )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("[", signature(x="Image", i="missing", j="numeric"),
  function(x, i, j, k, ..., drop) {
    if (missing(k)) k = 1:(dim(x@.Data)[3])
    tmp = x@.Data[ , j, k, drop=FALSE]
    if ( !is.array(tmp) ) return (tmp)
    res <- header (x)
    imageData (res) <- tmp
    return (res)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("[", signature(x="Image", i="numeric", j="numeric"),
  function (x, i, j, k, ..., drop) {
    if (missing(k)) k = 1:( dim(x@.Data)[3] )
    tmp = x@.Data[i, j, k, drop = FALSE]
    if ( !is.array(tmp) ) return (tmp)
    res <- header (x)
    imageData (res) <- tmp
    return (res)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("[", signature(x="Image", i="array", j="missing"),
  function (x, i, j, k, ..., drop) {
    if ( !missing(k) ) stop( "array index cannot be combined with any other index" )
      tmp = x@.Data[i, drop=FALSE]
      if ( !is.array(tmp) ) return (tmp)
      res <- header (x)
      imageData (res) <- tmp
      return (res)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("[", signature(x="Image", i="logical", j="missing"),
  function (x, i, j, k, ..., drop) {
    if ( !missing(k) ) stop( "logical index cannot be combined with any other index" )
    tmp = x@.Data[i, drop=FALSE]
    if ( !is.array(tmp) ) return (tmp)
    res <- header (x)
    imageData (res) <- tmp
    return (res)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("show", signature(object="Image"),
  function (object) {
    cat ("\n'", class(object),"'\n", sep="")
    
    if ( colorMode(object) == TrueColor ) {
      cat ("  colorMode()   : TrueColor\n")
      cat ("  storage class : integer 3D array, 8-bit/color RGB-, no alpha\n")
    }
    if ( colorMode(object) == Grayscale ) {
      cat ("  colorMode()   : Grayscale\n")
      cat ("  storage class : numeric 3D array, writable images in range [0..1]\n")
    }
    dimobject = dim (object)
    if ( dimobject[3] > 1 )
      cat ( sprintf ("  dim()         : %dx%d, %d image(s)\n", dimobject[1], dimobject[2], dimobject[3]) )
    else
      cat ( sprintf ("  dim()         : %dx%d\n", dimobject[1], dimobject[2]) )

    nd <- dimobject[1:2]
    if ( nd[1] > 5 ) nd[1] <- 5
    if ( nd[2] > 6 ) nd[2] <- 6
    for ( i in seq_len(dimobject[3]) ) {
      cat( "\nimage ", i, "/", dimobject[3], ":\n", sep="" )
      print( object@.Data[seq_len(nd[1]), seq_len(nd[2]), i] )
      if ( any(nd != dimobject[1:2]) ) cat(" ...\n")
    }
    invisible(NULL)
  }
)

print.Image <- function(x, ...) show(x)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("image", signature(x="Image"),
  function(x, i, xlab = "", ylab = "", axes = FALSE, col=gray ((0:255) / 255), ...) {
    dimx <- dim (x)
    if ( missing(i) ) {
      if ( dimx[3] > 1 ) warning( "missing i for an image stack, assuming i=1" )
      i <- 1
    }
    i <- as.integer ( i[1] )
    if ( i < 1 || i > dimx[3] )
      stop( "index 'i' out of range" )
    if ( any(dimx == 0) )
      stop( "image size is zero, nothing to plot" )
    X <- 1:dimx[1]
    Y <- 1:dimx[2]
    Z <- imageData(x[,,i])[, rev(Y), 1, drop=TRUE]
    asp <- dimx[2] / dimx[1]
    graphics:::image (x=X, y=Y, z=Z, asp=1, col=col, axes=axes, xlab=xlab, ylab=ylab, ...)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("channel", signature(x="Image", mode="character"),
  function (x, mode, ...) {
    mode <- tolower (mode)
    modeNo <- as.integer( switch (EXPR=mode, rgb=0, grey=, gray=1, r=, red=2, g=,
              green=3, b=, blue=4, asred=5, asgreen=6, asblue=7, x11=8, -1) )
    if (modeNo < 0)
      stop("wrong conversion mode")
    if (is(x, "IndexedImage")) {
      x = normalize(x)
      warning("IndexedImage 'x' is normalized to [0,1] prior to applying the filter")
    }      
    resData <- .DoCall("lib_channel", x@.Data, modeNo)
    if (is.null(resData))
      stop("error converting colors, check if all supplied values majke sense for color representation")
    resData[which(is.na(x))] = NA
    resData = array (resData, dim(x) )
    if (mode == "x11") return(resData)
    res <- header(x)
    res@colormode <- switch (EXPR=mode, rgb=, asred=, asgreen=,
                     asblue=TrueColor, Grayscale)
    res@.Data <- resData
    return(res)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("hist", signature(x="Image"),
  function (x, breaks=64L, main=paste("Image histogram:", length(x), "pixels"), xlab="Intensity", ...) {

    if ( colorMode(x) == TrueColor ) {
      colores = c("red", "green", "blue")
      y = lapply(colores, function(m) imageData(channel(x, m)))
      names(y) = colores
    } else {
      y = list(black=imageData(x))
    }
    
    rg = range(unlist(y), na.rm=TRUE)
    if(length(breaks)==1L) {
      dr = (rg[2]-rg[1])/(breaks*2L+2L)
      breaks = seq(rg[1]-dr, rg[2]+dr, length=breaks+1L)
    }
    
    h = lapply(y, hist, breaks=breaks, plot=FALSE)
    px = sapply(h, "[[", "breaks")[-1L,,drop=FALSE]
    matplot(x = px + dr*(col(px)-ncol(px)/2)/2,
            y = sapply(h, "[[", "counts"), type="s", lty=1L, 
            main=main, xlab=xlab, col=names(y), ylab="counts", ...)
  }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("combine", signature(x="Image", y="Image"),
  function (x, y, ...) {
    if ( !assert(x, y) )
      stop( "images must have the same size and color mode" )
    nz <- dim(x)[3] + dim(y)[3]
    args <- list ( ... )
    if ( length(args) > 0 ) {
      for (i in seq_along(args ) ) {
        stopIfNotImage ( args[[i]] )
        if ( !assert(x, args[[i]]) )
          stop( "images must have the same size and color mode" )
        nz <- nz + dim(args[[i]])[3]
      }
    }
    res <- header(x)
    if (colorMode(x) == TrueColor)
      res@.Data <- array(as.integer(c(x,y,...)), c(dim(x)[1:2],nz))
    else
      res@.Data <- array(as.double(c(x,y,...)), c(dim(x)[1:2],nz))
    return (res)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("combine", signature(x="list", y="missing"),
  function (x, y, ...) {
    names(x) <- NULL
    do.call("combine", x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("tile", signature(x="Image"),
  function (x, nx=10, lwd=1, fg.col="#E4AF2B", bg.col="gray", ...) {
    if ( nx < 1 || lwd < 0 || lwd > 100 )
      stop( "wrong range of arguments, see help for range details" )
    if ( colorMode(x) == TrueColor ) cols <- channel(c(fg.col,bg.col), "rgb")
    else cols <- channel(c(fg.col,bg.col), "gray")
    hdr <- header(x)
    hdr@.Data <- array(cols, c(2,1,1))
    x = .DoCall("lib_tile_stack", x, hdr, as.integer(c(nx, lwd)) )
    if (is(x, "IndexedImage")) x = as.Image(x)
    return(x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("untile", signature(x="Image", nim="numeric"),
  function (x, nim, lwd=1, ...) {
    nim = as.integer(nim)
    lwd = as.integer(lwd)
    if (length(nim)<2)
      stop("'nim' must contain two values for the number of images in x and y directions")
    if (lwd<0)
      stop("wrong line width")
    return(.DoCall("lib_untile", x, header(x), nim, lwd))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
median.Image <- function(x, na.rm = FALSE) {
  median(imageData(x), na.rm=na.rm)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rgbImage = function(r,g,b) {
  res = list()
  if (!missing(r)) res$r = channel(r,"asred")
  if (!missing(g)) res$g = channel(g,"asgreen")
  if (!missing(b)) res$b = channel(b,"asblue")
  if (length(res)==0)
    stop("at least one image must be provided")
  dim1 = dim(res[[1]])
  for (i in seq_along(res))
    if (any(dim(res[[i]])!=dim1))
      stop("all images must have the same dimensions")
  switch(length(res),
    res[[1]],
    res[[1]]+res[[2]],
    res[[1]]+res[[2]]+res[[3]]
  )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
channelMix = function(r,g,b) {
  mix = list()
  if (!missing(r)) mix[["r"]] = r
  if (!missing(g)) mix[["g"]] = g
  if (!missing(b)) mix[["b"]] = b
  if (length(mix)==0)
    stop("at least one image must be provided")
  if (length(mix)==1) return(mix[[1]])
  res = mix[[1]]^2 + mix[[2]]^2
  if (length(mix)==3) res = res + mix[[3]]^2
  sqrt(res)
}

