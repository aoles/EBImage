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
setMethod ("normalize", signature(x="Image"),
  function (x, separate=TRUE, ft=c(0,1), ...) {
    if ( colorMode(x) != Grayscale )
      stop( "'x' must be Grayscale, use 'normalize2' for TrueColor images" )
    ft <- as.numeric (ft)
    if ( diff(ft) == 0 )
      stop( "normalization range is 0" )
    separate <- as.integer (separate)
    return ( .DoCall("lib_normalize", x, separate, ft) )
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
# ImageMagick filters below
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
    function (x, type="G", ...) {
        type = tolower (substr(type,1,1))
        param = as.numeric( switch(type, u= 1, g= 2, m= 3, i= 4, l= 5, p= 6, 2 ) )
        if ( param == 2 && type != "g" )
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
setMethod ("normalize2", signature(x="Image"),
    function (x, ...) {
        .DoCall("lib_filterMagick", x, flt.norm, as.numeric(0) )
    }
)


