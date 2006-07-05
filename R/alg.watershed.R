# -------------------------------------------------------------------------
# Watershed algorithm of object detection
# Bundled with src/alg_watershed.{h,cpp}
 
# Copyright (c) 2006 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License 
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.          

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

# See the GNU General Public License for more details.
# GPL license wording: http://www.gnu.org/licenses/gpl.html

# -------------------------------------------------------------------------


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# The real implementation of the watershed algorithm
# Breaks R rules and modifies the original image, for internal use only
# use watershed() instead!
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.watershed <- function(x, mind=15, minr=10, ef=0.2, seeds=NULL, ref=NULL, modify=TRUE) {
    if(!assert(x))
        stop("wrong class of argument 'x'")
    if (x@rgb)
        stop("'x' must be a distance map (which are grayscales)")
    if (!is.null(ref)) {
        if(!assert(ref))
            stop("wrong class of argument 'ref'")
        if (ref@rgb)
            stop("'ref' must be grayscale")
        if (length(which(dim(x)!= dim(ref))) > 0)
            stop("'x' and 'ref' have different sizes")
    }
    nimg <- dim(x)[[3]]
    if (!is.null(seeds)) {
        if (nimg == 1 && !is.matrix(seeds))
            stop("'seeds' must be a 2D numeric matrix for a single 2D image")
        if (nimg > 1 && !is.list(seeds))
            stop("'seeds' must be a list of 2D numeric matrices for a stack of images")
    }
    mind <- as.double(mind)
    minr <- as.double(minr)
    if (mind < 0 || minr < 0)
        stop("'mindist' and 'minr' must be non-negative")
    ef <- as.double(ef)
    if (ef < 0 || ef > 1)
        warning("reasonable range for 'ef' is [0,1]")
    if (!modify)
        x <- copy(x)
    res <- .CallEBImage("watershed", x, ref, seeds, c(mind, minr, ef))
    return(res)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Safe exported implementation of watershed (uses modify = FALSE in .watershed)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
watershed <- function(x, mind=15, minr=10, ef=0.2, seeds=NULL, ref=NULL) {
    .watershed(x, mind, minr, ef, seeds, ref, FALSE)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Paints img with objects detected with watershed by a set of provided colors
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
paintws <- function(x, img, opac=0.2, col="default", fill=TRUE, brds=TRUE) {
    if(!assert(img))
        stop("wrong class of argument img")
    opac <- as.double(opac)
    if (opac < 0 || opac > 1)
        stop("'opac' must be in the range [0,1]")
    res <- scale2RGB(img, 1.0 - opac)
    if (!fill && !brds)
        return(res)
    nimg <- dim(img)[[3]]
    if (col == "default")
        col <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
    colRamp <- colorRamp(col)
    cols <- list()
    for (i in 1:nimg) {
        nobj <- 0
        if (nimg > 1) {
            if (!is.null(x[[i]]$objects))
                nobj <- length(x[[i]]$objects[,1])
        }
        else {
            if (!is.null(x$objects))
                nobj <- length(x$objects[,1])
        }
        if (nobj > 0) {
            mcol <- colRamp(((1:nobj) - 1) / (nobj - 1)) / 256
            cols[[i]] <- toRed(mcol[,1]) + toGreen(mcol[,2]) + toBlue(mcol[,3])
        } 
        else 
            cols[[i]] = NULL    
    }
    fill <- as.logical(fill)
    brds <- as.logical(brds)
    return(.CallEBImage("paintws", x, res, cols, fill, brds, opac))
}
