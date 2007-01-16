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
    function (x, ext=1, alg="exclude", ..., verbose=FALSE) {
        if ( colorMode(x) != Grayscale )
            stop ( .("only Grayscale images are supported, use 'channel' to convert") )
        alg <- switch(EXPR=tolower(alg), exclude=0, steepest=1, smooth=2, -1)
        if ( alg < 0 )
            stop ( .("possible values for 'alg' are 'exclude', 'steepest' and 'smooth'") )
        if ( as.integer(ext) < 1 )
            stop ( .("ext must be a positive integer value") )
        return( .DoCall("lib_filterInvWS", x, as.integer(alg), as.integer(ext), as.integer(verbose) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("features", signature (x="Image"),
    function (x, ...) x@features
)

setReplaceMethod ("features", signature (x="Image", value="list"),
    function (x, ..., value) {
        error = FALSE
        if ( !is.list(value) )
            stop ( .("'value' must be a list") )
        if ( length(value) != dim(x)[3] ) 
            stop ( .("'value' length must be the same as number of images") )
        for ( i in 1:length(value) )
            if ( !is.matrix(value[[i]]) && !is.null(value[[i]]) )
                stop ( .("all list items must be matrices") )
        x@features <- value
        return (x)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("getObjects", signature(x="Image", ref="Image"),
    function (x, ref, ...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("only Grayscale images are supported, use 'channel' to convert") )
        if ( !assert(x, ref, strict=TRUE) )
            stop ( .("'x' and 'ref' must be of the same size and color mode") )
        res <- .DoCall ("lib_getFeatures", x, ref)
        if ( is.list(res) ) {
            ## set colnames for features
            for ( i in 1:length(res) ) 
                if ( is.matrix( res[[i]] ) )
                    if ( ncol(res[[i]] ) == 11 )
                       colnames( res[[i]] ) <- c("x", "y", "size", "per", "image.border", "effr", "intensity", "acirc", "per.mean", "per.sd", "per.by.2.pi.effr")
        }
        return (res)
    }
)

setMethod ("getObjects", signature(x="Image", ref="missing"),
    function (x, ref, ...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("only Grayscale images are supported, use 'channel' to convert") )
        res <- .DoCall ("lib_getFeatures", x, NULL)
        if ( is.list(res) ) {
            ## set colnames for features
            for ( i in 1:length(res) ) 
                if ( is.matrix( res[[i]] ) )
                    if ( ncol(res[[i]] ) == 11 )
                       colnames( res[[i]] ) <- c("x", "y", "size", "per", "image.border", "effr", "intensity", "acirc", "per.mean", "per.sd", "per.by.2.pi.effr")
        }
        return (res)
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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("combineObjects", signature(x="Image"),
    function (x, ext=1, fraction=0.3, seeds=NULL,...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("only Grayscale images are supported, use 'channel' to convert") )
        if ( as.integer(ext) < 1 )
            stop ( .("ext must be a positive integer value") )
        if ( fraction <= 0 || fraction > 1 )
            stop ( .("'fraction' must be in the range (0,1]") )
        if ( !is.null(seeds) )
            seeds <- lapply ( as.list(seeds), as.integer )
        return ( .DoCall("lib_combineFeatures", x, as.integer(ext), as.numeric(fraction), seeds ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("matchObjects", signature(x="Image", ref="Image"),
    function (x, ref, ...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("only Grayscale images are supported, use 'channel' to convert") )
        if ( !assert(x, ref, strict=TRUE) )
            stop ( .("'x' and 'ref' must be of the same size and both Grayscale") )
        return ( .DoCall ("lib_matchFeatures", x, ref) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("rmObjects", signature(x="Image", index="list"),
    function (x, index, ext=1, ...) {
        if ( colorMode(x) != Grayscale )
            stop ( .("only Grayscale images are supported, use 'channel' to convert") )
        if ( as.integer(ext) < 1 )
            stop ( .("ext must be a positive integer value") )
        index <- lapply (index, as.integer)
        return ( .DoCall ("lib_deleteFeatures", x, index, as.integer(ext) ) )
    }
)

