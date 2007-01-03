# Feature and object detection functions and related algorithms

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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("getObjects", signature(x="Image", ref="Image"),
    function (x, ref, ...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("only Grayscale images are supported, use 'channel' to convert") )
        if ( !assert(x, ref, strict=TRUE) )
            stop ( .("'x' and 'ref' must be of the same size and color mode") )
        return ( .DoCall ("lib_assignFeatures", x, ref) )
    }
)

setMethod ("getObjects", signature(x="Image", ref="NULL"),
    function (x, ref, ...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("only Grayscale images are supported, use 'channel' to convert") )
        return ( .DoCall ("lib_assignFeatures", x, NULL) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("paintObjects", signature(x="Image", tgt="Image"),
    function (x, tgt, opac=c(0.4, 0.05, 0.4), col=c("#FFC72C","#5BABF6","#FF372C"), ...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("only Grayscale images are supported, use 'channel' to convert") )
        if ( any( dim(x) != dim(tgt) ) )
            stop ( .("'x' and 'ref' must have equal size, 'ref' can be TrueColor") )
        if ( length(opac) < 3 || length(col) < 3 )
            stop ( .("vectors 'opac' and 'col' must have at least 3 elements each: edge color and opacity, background, object contact") )
        if ( any(opac < 0) || any(opac > 1) )
            stop ( .("all opacity values must be in the range [0,1]") )
        return ( .DoCall("lib_paintFeatures", x, tgt, as.numeric(opac), as.character(col)) )
    }
)
