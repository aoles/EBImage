# Class Image, definition and methods

# Copyright (c) 2005 Oleg Sklyar

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
        colormode    = "integer",    ## 0 - gray, 1 - RGB etc
        filename     = "character",
        compression  = "character",  ##
        resolution   = "numeric",    ## length = 2 ## lost in jpeg, pixels per inch
        filter       = "character",  ## filter for sampling
        features     = "list"
    ),
    prototype (
        colormode    = Grayscale,
        filename     = "no-name",
        compression  = "LZW",
        resolution   = c(2.5e+6, 2.5e+6), ## 1 px per 1um in pixels per inch
        filter       = "lanczos",
        features     = list()

    ),
    contains = "array"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Image <- function (data=0.5, dim=c(200,200), colormode=Grayscale, ...) {
    if ( is.null(dim) ) {
        if ( !is.array(data) )
            stop ( .("supply dim or convert data to 2D or 3D array") )
        dim = dim (data)
    }
    if ( length(dim) == 2 )
        dim <- c (dim, 1)
    if ( length(dim) != 3 )
        stop( .("length(dim) must be 2 for single images or 3 for stacks") )
    if (colormode == TrueColor)
        return ( new("Image", colormode=TrueColor, .Data=array( as.integer(data), dim), ... ) )
    else
        return ( new("Image", colormode=Grayscale, .Data=array( as.double(data), dim), ... ) )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("colorMode", signature (x="Image"),
    function (x, ...) x@colormode
)
setReplaceMethod ("colorMode", signature (x="Image", value="numeric"),
    function (x, ..., value) {
        if ( value == x@colormode ) return (x)
        if ( value == TrueColor ) return ( channel(x, "RGB") )
        if ( value == Grayscale ) return ( channel(x, "gray") )
        # on any other unsupported value
        return (x)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("fileName", signature (x="Image"),
    function (x, ...) x@filename
)
setReplaceMethod ("fileName", signature (x="Image", value="character"),
    function (x, ..., value) {
        x@filename <- value
        return (x)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("compression", signature (x="Image"),
    function (x, ...) x@compression
)
setReplaceMethod ("compression", signature (x="Image", value="character"),
    function (x, ..., value) {
        value <- toupper (value)
        if ( switch (EXPR=value, NONE=, LZW=, ZIP=, JPEG=, BZIP=, GROUP4=, FALSE, TRUE) )
            stop ( .("wrong compression type. Please specify, NONE, LZW, ZIP, JPEG, BZIP or GROUP4") )
        x@compression <- value
        return (x)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("resolution", signature (x="Image"),
    function (x, ...) x@resolution
)
setReplaceMethod ("resolution", signature (x="Image", value="numeric"),
    function (x, ..., value) {
        if ( length(value) != 2 )
            stop ( .("resolution attribute needs 2 values, for x and y in pixels per inch") )
        if ( any( value <= 0 ) )
            stop ( .("resolution must be positive") )
        x@resolution <- value
        return (x)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("sampleFilter", signature (x="Image"),
    function (x, ...) x@filter
)
setReplaceMethod ("sampleFilter", signature (x="Image", value="character"),
    function (x, ..., value) {
        value <- tolower (value)
        if ( switch (EXPR=value, point=, box=, triangle=, hermite=, hanning=,
                hamming=, blackman=, gaussian=, quadratic=, cubic=, catrom=, mitchell=,
                lanczos=, bessel=, sinc=FALSE, TRUE) )
            stop ( paste( .("wrong filter type. Possible values are:"), "point, box, triangle, hermite, hanning, hamming, blackman, gaussian, quadratic, cubic, catrom, mitchell, lanczos, bessel, sinc") )
        x@filter <- value
        return (x)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("imageData", signature (x="Image"),
    function (x, ...) x@.Data
)
setReplaceMethod ("imageData", signature (x="Image", value="matrix"),
    function (x, ..., value) {
        dim (value) <- c( dim(value), 1)
        x@.Data <- value
        return (x)
    }
)
setReplaceMethod ("imageData", signature (x="Image", value="array"),
    function (x, ..., value) {
        diml <- length ( dim(value) )
        if ( diml < 2 || diml > 3 )
            stop ( .("supplied array must be 2 or 3 dimensional") )
        if ( diml == 2 )
        dim (value) <- c( dim(value), 1)
        x@.Data <- value
        return (x)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is.Image <- function (x) {
    if ( !is(x, "Image") ) return (FALSE)
    if ( length( dim(x) ) != 3) return (FALSE)
    return (TRUE)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("assert", signature (x="Image", y="Image"),
    function (x, y, strict=FALSE, ...) {
        n <- 2
        if ( !missing(strict) )
            if ( strict )
                n <- 3
        if ( any( dim(x)[1:n] != dim(y)[1:n] ) || colorMode(x) != colorMode(y) ) return (FALSE)
        return (TRUE)
    }
)
setMethod ("assert", signature (x="Image", y="missing"),
    function (x, y, ...) is.Image (x)
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stopIfNotImage <- function (x) {
    if ( !is(x, "Image") )
        stop ( .("argument must be of class 'Image'") )
    if ( length( dim(x) ) != 3)
        stop ( .("array dimensions for the object of class 'Image' must be 3") )
    invisible (NULL)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("header", signature(x="Image"),
    function (x, ...) {
        if ( x@colormode == TrueColor )
            return ( new("Image", colormode=TrueColor, .Data=integer(0), filename=fileName(x),
                compression=compression(x), resolution=resolution(x),
                filter=sampleFilter(x), features=features(x) ) )
        return ( new("Image", colormode=Grayscale, .Data=numeric(0), filename=fileName(x),
            compression=compression(x), resolution=resolution(x),
            filter=sampleFilter(x), features=features(x) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("copy", signature (x="Image"),
    function (x, ...) {
        res = header (x)
        if ( x@colormode == TrueColor )
            res@.Data = array (as.integer(x@.Data), dim(x@.Data) )
        else
            res@.Data = array (as.double(x@.Data), dim(x@.Data) )
        return(res)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod (".isCorrectType", signature(x="Image"),
    function (x) {
        if ( x@colormode == TrueColor && !is.integer(x) ) {
            warning( .("RGB image data stored as numeric") )
            return (FALSE)
        }
        if ( x@colormode == Grayscale && !is.double(x)) {
            warning( .("grayscale image data stored as integer") )
            return (FALSE)
        }
        return (TRUE)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod (".correctType", signature(x="Image"),
    function(x) {
        if ( x@colormode == TrueColor && !is.integer(x) )
            x@.Data = array (as.integer(x@.Data), dim(x@.Data) )
        if ( x@colormode == Grayscale && !is.double(x) )
            x@.Data = array (as.double(x@.Data), dim(x@.Data) )
        return (x)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("display", signature(x="Image"),
    function (x, no.GTK=FALSE, ...) {
        if ( !.isCorrectType(x) )
            x <- .correctType (x)
        invisible ( .DoCall("lib_display", x, as.logical(no.GTK) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("animate", signature(x="Image"),
    function (x, ...) {
        if ( !.isCorrectType(x) )
            x <- .correctType (x)
        invisible ( .DoCall("lib_animate", x ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("write.image", signature(x="Image", files="character"),
    function (x, files, quality, ...) {
        if ( missing(quality) )
            quality <- 90
        else
            quality <- as.integer (quality)
        if ( quality < 1 || quality > 100 )
            stop ( .("quality value is given in % between 1 and 100") )
        if ( !.isCorrectType(x) )
            x <- .correctType (x)
        invisible ( .DoCall("lib_writeImages", x, as.character(files), as.integer(quality) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("write.image", signature(x="Image", files="missing"),
    function (x, files, quality, ...) {
        if ( missing(quality) )
            quality <- 90
        else
            quality <- as.integer (quality)
        if ( quality < 1 || quality > 100 )
            stop ( .("quality value is given in % between 1 and 100") )
        if ( !.isCorrectType(x) )
            x <- .correctType (x)
        invisible ( .DoCall("lib_writeImages", x, fileName(x), as.integer(quality) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read.image <- function(files, colormode=Grayscale, ...) {
    .DoCall ("lib_readImages", as.character(files), as.integer(colormode) )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
choose.image <- function() {
    .DoCall ("lib_chooseImages")
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("[", signature(x="Image", i="missing", j="missing"),
    function (x, i, j, k, ..., drop) {
        if ( missing(k) )
            return (x)
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
        ## Use nargs() to decide between [i], [i,] and [i,,]/[i,,k]
        ## For [i], just return a numeric vector
        ## For [i,,]/[i,,k], return subset Image objects
        ## For [i,] return error
        n <- nargs()
        if ( n == 2L )
            return( x@.Data[i] )
        if ( n != 4L )
            stop ( .("incorrect number of dimensions") )
        res <- header(x)
        imageData (res) <- x@.Data[i,,...,drop=FALSE]
        return( res )
   }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("[", signature(x="Image", i="missing", j="numeric"),
    function(x, i, j, k, ..., drop) {
        if (missing(k))
            k = 1:(dim(x@.Data)[3])
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
        if (missing(k))
            k = 1:( dim(x@.Data)[3] )
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
        if ( !missing(k) )
            stop ( .("array index cannot be combined with any other index") )
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
        if ( !missing(k) )
            stop ( .("logical index cannot be combined with any other index") )
        tmp = x@.Data[i, drop=FALSE]
        if ( !is.array(tmp) ) return (tmp)
        res <- header (x)
        imageData (res) <- tmp
        return (res)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
print.Image <- function(x, ...) {
    if ( length( features(x) ) > 0 )
        cat ("\n'Image' with annotated object data\n")
    else
        cat ("\n'Image'\n")
    if ( colorMode(x) == TrueColor ) {
        cat ("  colorMode()   : TrueColor\n")
        cat ("  storage class : integer 3D array, 8-bit/color RGB-, no Alpha\n")
    }
    if ( colorMode(x) == Grayscale ) {
        cat ("  colorMode()   : Grayscale\n")
        cat ("  storage class : numeric 3D array, writable images in range [0..1]\n")
    }
    dimx = dim (x)
    if ( dimx[3] > 1 )
        cat ( sprintf ("  dim()         : %dx%d, %d image(s)\n", dimx[1], dimx[2], dimx[3]) )
    else
        cat ( sprintf ("  dim()         : %dx%d\n", dimx[1], dimx[2]) )
    cat ( sprintf ("  fileName()    : %s \n", fileName(x) ) )
    cat ( sprintf ("  compression() : %s \n", compression(x) ) )
    cat ( sprintf ("  resolution()  : dx = %.1f, dy = %.1f \n", resolution(x)[1], resolution(x)[2]) )
    cat ( sprintf ("  sampleFilter(): %s \n", sampleFilter(x) ) )

    if ( length( features(x) ) > 0 ) {
        cat ( "\n  Object data: features()\n" )
        print ( x@features )
        return ( invisible(NULL) )
    }

    partial <- rep(FALSE, 3)
    dmax <- c(5, 6, 3)
    for (i in 1:3) {
        if ( dimx[i] > dmax[i]) {
            dimx[i] <- dmax[i]
            partial[i] = TRUE
        }
    }
    cat ( sprintf("\n  imageData() subset [1:%d, 1:%d, 1:%d]:\n", dimx[1], dimx[2], dimx[3]) )
    print ( x@.Data[ 1:dimx[1], 1:dimx[2], 1:dimx[3] ], digits=3 )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("show", signature(object="Image"),
    function (object) print (object)
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## FIXME: as.matrix causes incompatibility with R < 2.4.1, as.array disabled as well, use imageData()
#setMethod ("as.array", signature(x="Image"),
#    function (x) imageData (x)
#)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## FIXME: as.matrix causes incompatibility with R < 2.4.1, use imageData(as.matrix())
#setMethod ("as.matrix", signature(x="Image"),
#    function (x, ...) {
#        if ( dim(x)[3] > 1 )
#            stop ( .("cannot coerce multiple images to matrix") )
#        return ( matrix( imageData(x), dim(x)[1:2] ) )
#    }
#)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
image.Image <- function(x, i, xlab = "", ylab = "", axes = FALSE, col=gray ((0:255) / 255), ...) {
    dimx <- dim (x)
    if ( missing(i) ) {
        if ( dimx[3] > 1 )
            warning ( .("missing i for an image stack, assuming i=1") )
        i <- 1
    }
    i <- as.integer ( i[1] )
    if ( i < 1 || i > dimx[3] )
        stop ( .("index i out of range") )
    if ( any(dimx == 0) )
        stop ( .("image size is zero, nothing to plot") )
    X <- 1:dimx[1]
    Y <- 1:dimx[2]
    Z <- as.matrix ( imageData(x[,,i]) )[, rev(Y)]
    asp <- dimx[2] / dimx[1]
    graphics:::image (x=X, y=Y, z=Z, asp=asp, col=col, axes=axes, xlab=xlab, ylab=ylab, ...)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("channel", signature(x="Image", mode="character"),
    function (x, mode, ...) {
        mode <- tolower (mode)
        if ( mode == "grey" ) mode = "gray"
        modeNo <- switch (EXPR=mode, rgb=0, gray=1, red=2, green=3,
                blue=4, asred=5, asgreen=6, asblue=7, x11=8, -1)
        if ( modeNo < 0 )
            stop ( paste(.("wrong conversion mode. Please specify one of"), "rgb, gray, grey, red, green, blue, asred, asgreen, asblue, x11") )
        resData <- .DoCall("lib_channel", x@.Data, as.integer(modeNo) )
        if ( is.null(resData) )
            stop ( .("could not convert colors, NULL result") )
        resData [ which( is.na(x) ) ] = NA
        resData <- array (resData, dim(x) )
        if ( mode == "x11" ) return (resData)
        res <- header (x)
        res@colormode = switch (EXPR=mode, rgb=, asred=, asgreen=, asblue=TrueColor, Grayscale)
        res@.Data <- resData
        return (res)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
hist.Image <- function (x, breaks=255, main=paste("Image histogram. Total", length(x), "pixels"), xlab="colors", ...) {
    if ( xlab == "colors" ) {
        if ( colorMode(x) == Grayscale )
            xlab = "Shades of gray, 0: black, 1: white"
        if ( colorMode(x) == TrueColor )
            xlab = "RGB values, non-informative"
    }
    graphics:::hist ( imageData(x), breaks=breaks, main=main, xlab=xlab, ...)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("combine", signature(x="Image", y="Image"),
    function (x, y, ...) {
        if ( !assert(x, y) )
            stop ( .("images must have the same size and color mode") )
        nz <- dim(x)[3] + dim(y)[3]
        args <- list ( ... )
        if ( length(args) > 0 ) {
            for (i in seq_along(args ) ) {
                stopIfNotImage ( args[[i]] )
                if ( !assert(x, args[[i]]) )
                    stop ( .("images must have the same size and color mode") )
                nz <- nz + dim(args[[i]])[3]
            }
        }
        res <- header (x)
        if ( colorMode(x) == TrueColor )
            res@.Data <- array ( as.integer( c(x, y, ...) ), c( dim(x)[1:2], nz ) )
        else
            res@.Data <- array ( c(x, y, ...), c( dim(x)[1:2], nz ) )
        return (res)
    }
)
