# Filter methods for class Image

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


# filter integer constants
flt.blur      <- as.integer(0)
flt.gaussblur <- as.integer(1)
flt.contrast  <- as.integer(2)
flt.denoise   <- as.integer(3)
flt.despeckle <- as.integer(4)
flt.edge      <- as.integer(5)
flt.enhance   <- as.integer(6)
flt.equalize  <- as.integer(7)
flt.gamma     <- as.integer(8)
flt.median    <- as.integer(9)
flt.noise     <- as.integer(10)
flt.resize    <- as.integer(11)
flt.rotate    <- as.integer(12)
flt.sample    <- as.integer(13)
flt.segment   <- as.integer(14)
flt.sharpen   <- as.integer(15)
flt.unsharp   <- as.integer(16)
flt.athresh   <- as.integer(17)
flt.cthresh   <- as.integer(18)
flt.affinet   <- as.integer(19)
flt.modulate  <- as.integer(20)
flt.negate    <- as.integer(21)
flt.norm      <- as.integer(22)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("blur", signature(x="Image"),
    function (x, r=0, s=0.5, ...) {
        if ( r <= s && r != 0 )
            warning ( .("for reasonable results, 'r' should be larger than 's'") )
        if ( r < 0 || s <= 0 )
            stop ( .("values of 'r' and 's' must be positive, alternatively r=0 leads to automatic radius selection") )
        return ( .DoCall("lib_filterMagick", x, flt.blur, as.numeric( c(r, s) ) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("gblur", signature(x="Image"),
    function (x, r=0, s=0.5, ...) {
        if ( r <= s && r != 0)
            warning ( .("for reasonable results, 'r' should be larger than 's'") )
        if ( r < 0 || s <= 0 )
            stop ( .("values of 'r' and 's' must be positive, alternatively r=0 selects radius automatically") )
        return ( .DoCall("lib_filterMagick", x, flt.gaussblur, as.numeric( c(r, s) ) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("contrast", signature(x="Image"),
    function (x, sharpen=TRUE, ...)
        .DoCall ("lib_filterMagick", x, flt.contrast, as.numeric(sharpen) )
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("denoise", signature(x="Image"),
    function (x, r=0, ...) {
        if ( r < 0 )
            stop ( .("'r' must be positive, alternatively r=0 selects radius automatically") )
        return ( .DoCall("lib_filterMagick", x, flt.denoise, as.numeric(r) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("despeckle", signature(x="Image"),
    function (x, ...)
        .DoCall ("lib_filterMagick", x, flt.despeckle, as.numeric(0) )
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("edge", signature(x="Image"),
    function (x, r=0, ...)
        .DoCall ("lib_filterMagick", x, flt.edge, as.numeric(r) )
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("enhance", signature(x="Image"),
    function (x, ...)
        .DoCall ("lib_filterMagick", x, flt.enhance, as.numeric(0) )
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("equalize", signature(x="Image"),
    function (x, ...)
        .DoCall ("lib_filterMagick", x, flt.equalize, as.numeric(0) )
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("cgamma", signature(x="Image"),
    function (x, level=1, ...) {
        if ( level < 0.8 || level > 2.3 )
            warning ( .("reasonable 'level' is between 0.8 and 2.3") )
        return ( .DoCall("lib_filterMagick", x, flt.gamma, as.numeric(level) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("mediansmooth", signature(x="Image"),
    function (x, r=2, ...) {
        if ( r <= 1 )
            stop ( .("value of 'r' must be larger than 1") )
        return ( .DoCall("lib_filterMagick", x, flt.median, as.numeric(r) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("noise", signature(x="Image"),
    function (x, type="gaussian", ...) {
        type = tolower (type)
        param = as.double( switch(type, "uniform" = 1, "gaussian" = 2,
            "multi" = 3, "impulse" = 4, "laplace" = 5, "poisson" = 6, 2 ) )
        if ( param == 2 && type != "gaussian" )
            warning ( .("unsupported noise type selected, using 'gaussian' instead. Possible values are: uniform, gaussian, multi, impulse, laplace and poisson" ) )
        return ( .DoCall("lib_filterMagick", x, flt.noise, as.numeric(param) ) )
    }
)
        
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("resize", signature(x="Image"),
    function (x, w, h, blur=1, ...) {
        if ( missing(w) && missing(h) )
            stop ( .("either 'w' or 'h' must be specified") )
        dimx = dim (x)
        if ( missing(w) )
            w <- dimx[1] * h / dimx[2]
        else
        if ( missing(h) )
            h <- dimx[2] * w / dimx[1]
        if ( w <= 0 || h <= 0 )
            stop ( .("width and height of a new image must be non zero positive") )
        return ( .DoCall("lib_filterMagick", x, flt.resize, as.numeric( c(w, h, blur) ) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("rotate", signature(x="Image"),
    function (x, angle=90, ...)
        .DoCall("lib_filterMagick", x, flt.rotate, as.numeric(angle) )
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("resample", signature(x="Image"),
    function (x, w, h, ...) {
        if ( missing(w) && missing(h) )
            stop ( .("either 'w' or 'h' must be specified") )
        dimx = dim (x)
        if ( missing(w) )
            w <- dimx[1] * h / dimx[2]
        else
        if ( missing(h) )
            h <- dimx[2] * w / dimx[1]
        if ( w <= 0 || h <= 0 )
            stop ( .("width and height of a new image must be non zero positive") )
        return ( .DoCall("lib_filterMagick", x, flt.sample, as.numeric( c(w, h) ) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("segment", signature(x="Image"),
    function (x, cl=10, s=1.5, ...) {
        if ( cl < 1 )
            stop ( .("cluster size 'cl' must be larger than 1") )
        if ( s <= 0 )
            stop ( .("smoothness 's' must be positive") )
        return ( .DoCall("lib_filterMagick", x, flt.segment, as.numeric( c(cl, s) ) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("sharpen", signature(x="Image"),
    function (x, r=0, s=0.5, ...) {
        if ( r <= s && r != 0 )
            warning ( .("for reasonable results, 'r' should be larger than 's'") )
        if ( r < 0 || s <= 0 )
            stop ( .("values of 'r' and 's' must be positive, alternatively r=0 selects radius automatically") )
        return ( .DoCall("lib_filterMagick", x, flt.sharpen, as.numeric( c(r, s) ) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("umask", signature(x="Image"),
    function (x, r=0, s=0.5, amount=5, t=2, ...) {
        if ( r <= s && r != 0 )
            warning ( .("for reasonable results, 'r' should be larger than 's'") )
        if ( r < 0 || s <= 0 || amount < 0 || t < 0 )
            stop ( .("all arguments must be positive, alternatively r=0 selects radius automatically") )
        return ( .DoCall("lib_filterMagick", x, flt.unsharp, as.numeric( c(r, s, amount, t) ) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## half width and height: moving frame will be 2 times + 1 px larger
setMethod ("thresh", signature(x="Image"),
    function (x, w=5, h=5, offset=0.01, ...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("'thresh' is only defined for grayscale images, use 'athresh' instead or 'channel' to convert") )
        if ( w < 2 || h < 2 )
            stop ( .("width 'w' and height 'h' must be larger than 1") )
        return ( .DoCall("lib_filterThresh", x, as.numeric( c(w, h, offset) ) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("athresh", signature(x="Image"),
    function (x, w=10, h=10, offset=0, ...) {
        if ( w < 2 || h < 2 )
            stop ( .("width 'w' and height 'h' must be larger than 1") )
        return ( .DoCall("lib_filterMagick", x, flt.athresh, as.numeric( c(w, h, offset) ) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("cthresh", signature(x="Image"),
    function (x, threshold=0, ...) {
        .DoCall("lib_filterMagick", x, flt.cthresh, as.numeric( threshold ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("affinet", signature(x="Image"),
    function (x, sx=0, rx=0, ry=0, sy=0, tx=0, ty=0, ...) {
        .DoCall("lib_filterMagick", x, flt.affinet, as.numeric( c(sx, rx, ry, sy, tx, ty) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("modulate", signature(x="Image"),
    function (x, value=100, ...) {
        .DoCall("lib_filterMagick", x, flt.modulate, as.numeric( value ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("negate", signature(x="Image"),
    function (x, ...) {
        .DoCall("lib_filterMagick", x, flt.negate, as.numeric(0) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## FIXME: normalize, add independent or similar option
setMethod ("normalize", signature(x="Image"),
    function (x, from=0, to=1, ...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("'normalize' is only defined for grayscale images, use 'normalize2' instead or 'channel' to convert") )
        if ( to[1] - from[1] == 0 )
            stop ( .("value 'to - from' must not be zero") )
        minx = min (x)
        maxx = max (x)
        if ( maxx - minx == 0 )
            stop ( .("image cannot be normalized as it contains one color only") )
        return ( (x - minx) / (maxx - minx) * (to - from) + from )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("normalize2", signature(x="Image"),
    function (x, ...) {
        .DoCall("lib_filterMagick", x, flt.norm, as.numeric(0) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("fill", signature(x="Image"),
    function (x, col, xoff, yoff, fuzz=10, method="floodfill", ...) {
        if ( missing(xoff) || missing(yoff) || missing(col) )
            stop ( .("missing 'xoff', 'yoff' or 'col'") )
        # color is converted into it's x11 representation, i.e. #112233
        col <- channel (col[1], "x11")
        xoff <- as.integer (xoff - 1)
        yoff <- as.integer (yoff - 1)
        if ( xoff < 0 || xoff >= dim(x)[1] || yoff < 0 || yoff >= dim(x)[2] )
            stop ( .("start point outside of image") )
        if ( fuzz < 0 )
            stop ( .("fuzz must be non negative") )
        method <- tolower(method)
        if ( !switch(method, floodfill=TRUE, replace=TRUE, FALSE) )
            stop ( .("wrong fill method specified, supported methods are: floodfill and replace") )
        return ( .DoCall("lib_filterFill", x, as.character(col), as.integer(xoff, yoff), as.character(method), as.integer(fuzz) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("flip", signature(x="Image"),
    function (x, ...) {
        Y <- 1:(dim(x)[2])
        x@.Data <- x@.Data[ , rev(Y), , drop=FALSE]
        return (x)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("flop", signature(x="Image"),
    function (x, ...) {
        X <- 1:(dim(x)[1])
        x@.Data <- x@.Data[rev(X), , , drop=FALSE]
        return (x)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
morphKern <- function (size=5, shape="round") {
    if ( size < 3 || ( size / 2 == as.integer(size / 2) ) )
        stop ( .("kernel size must be an odd number >= 3: [3, 5, 7, ...") )
    if ( switch(shape, round=, square=FALSE, TRUE) )
        stop("available shapes 'round' and 'square'")
    res <- matrix ( as.integer(FALSE), size, size, byrow = TRUE )
    cx = as.integer(size / 2) + 1
    if (shape == "round") {
        res[cx,] = as.integer(TRUE)
        res[,cx] = as.integer(TRUE)
        for ( i in 1:(cx-1) )
            for ( j in 1:(cx-1) )
                if ( (cx - i)^2 + (cx - j)^2 <= (cx - 1)^2 ) {
                    res[i, j] = as.integer (TRUE)
                    res[size - i + 1, j] = as.integer (TRUE)
                    res[i, size - j + 1] = as.integer (TRUE)
                    res[size - i + 1, size - j + 1] = as.integer (TRUE)
                }
        return (res)
    }
    # otherwise square
    res[] = as.integer (TRUE)
    return(res)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("erode", signature(x="Image"),
    function (x, kern=morphKern(5), iter=1, ...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("2-color bitmap images only are supported in grayscale format") ) ## FIXME
        if ( !is.integer(kern) || !is.matrix(kern) )
            stop ( .("kernel must be an integer matrix of 0's and 1's") )
        if ( iter < 1 )
            stop ( .("'iter' is assumed to be a positive integer") )
        return ( .DoCall("lib_erode_dilate", x, kern, as.integer(iter), as.integer(0) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("dilate", signature(x="Image"),
    function (x, kern=morphKern(5), iter=1, ...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("2-color bitmap images only are supported in grayscale format") ) ## FIXME
        if ( !is.integer(kern) || !is.matrix(kern) )
            stop ( .("kernel must be an integer matrix of 0's and 1's") )
        if ( iter < 1 )
            stop ( .("'iter' is assumed to be a positive integer") )
        return ( .DoCall("lib_erode_dilate", x, kern, as.integer(iter), as.integer(1) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("opening", signature(x="Image"),
    function (x, kern=morphKern(5), iter=1, ...)
        dilate ( erode(x, kern, iter, ...), kern, iter, ... )
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("closing", signature(x="Image"),
    function (x, kern=morphKern(5), iter=1, ...)
        erode ( dilate(x, kern, iter, ...), kern, iter, ... )
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("distmap", signature(x="Image"),
    function (x, tolerance=0.05, minBG=0.05, strict=FALSE, ...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("only Grayscale images are supported, use 'channel' to convert") )
        if ( tolerance < 0 || tolerance >= 1 )
            stop ( .("'tolerance' must be in the range [0,1)") )
        if ( minBG < 0 || minBG >= 1 )
            stop ( .("'minBG' must be in the range [0,1) and it is strongly recommended to keep this value at least as high as 0.05") )
        return ( .DoCall("lib_distMap", x, as.numeric(tolerance), as.numeric(minBG), as.integer(strict) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("watershed", signature(x="Image"),
    function (x, ref=NULL, do.detect=TRUE, ext=1, alg="exclude", ..., verbose=FALSE) {
        if ( colorMode(x) != Grayscale )
            stop ( .("only Grayscale images are supported, use 'channel' to convert") )
        if ( is.Image(ref) )
            if ( colorMode(ref) != Grayscale )
                stop ( .("only Grayscale images are supported in 'ref', use 'channel' to convert") )
        if ( !is.null(ref) && !is.Image(ref) )
            stop ( .("'ref' can be either an image of the same size as 'x' or a NULL") )
        if ( is.Image(ref) )
            if ( !assert(x, ref, strict=TRUE) )
                stop ( .("'ref' can be either an image of the same size as 'x' or a NULL") )
        alg <- switch(EXPR=tolower(alg), exclude=0, steepest=1, smooth=2, -1)
        if ( alg < 0 )
            stop ( .("possible values for 'alg' are 'exclude', 'steepest' and 'smooth'") )
        if ( as.integer(ext) < 1 )
            stop ( .("ext must be a positive integer value") )
        res <- .DoCall("lib_filterInvWS", x, ref, as.integer(do.detect), as.integer(alg), as.integer(ext), as.integer(verbose) )
        if ( is.Image(res) && do.detect ) {
            ## set colnames for features
            for ( i in 1:length(res@features) ) 
                if ( is.matrix( res@features[[i]] ) )
                    if ( ncol(res@features[[i]] ) == 6 )
                       colnames( res@features[[i]] ) <- c("x", "y", "size", "per", "int", "edge")
        }
        return (res)
    }
)

