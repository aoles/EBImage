# -------------------------------------------------------------------------
# Watershed algorithm of object detection
# Bundled with src/alg_watershed.{h,cpp}

# Copyright (c) 2006 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

# -------------------------------------------------------------------------


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# The real implementation of the watershed algorithm
# Breaks R rules and modifies the original image, for internal use only
# use watershed() instead!
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.wsObjects <- function(x, mind=15, minr=10, ef=0.2, seeds=NULL, ref=NULL, modify=TRUE) {
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
    res <- .CallEBImage("ws_objects", x, ref, seeds, c(mind, minr, ef))
    dnames <- c("x", "y", "size", "int", "per", "edge", "effr", "acirc", "acircint", "extmean", "extsd", "per2pr")
    if (nimg == 1)
        colnames(res$objects) <- dnames
    else {
        for (i in 1:nimg) colnames(res[[i]]$objects) <- dnames
    }
    return(res)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Safe exported implementation of watershed (uses modify = FALSE in .watershed)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
wsObjects <- function(x, mind=15, minr=10, ef=0.2, seeds=NULL, ref=NULL) {
    .wsObjects(x, mind, minr, ef, seeds, ref, FALSE)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Paints img with objects detected with watershed by a set of provided colors
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
wsPaint <- function(wsres, ref, opac=0.2, col="default", fill=TRUE, brds=TRUE) {
    if(!assert(ref))
        stop("wrong class of argument ref")
    opac <- as.double(opac)
    if (opac <= 0 || opac >= 1)
        stop("'opac' must be in the range (0,1)")
    if (fill)
        res <- scale2RGB(ref, 1.0 - opac)
    else
        res <- toRGB(ref)
    if (!fill && !brds)
        return(res)
    nimg <- dim(ref)[[3]]
    if (col[[1]] == "default")
        col <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
    if (length(col) == 1)
        col <- c(col, col)
    colRamp <- colorRamp(col)
    cols <- list()
    for (i in 1:nimg) {
        nobj <- 0
        if (nimg > 1) {
            if (!is.null(wsres[[i]]$objects))
                nobj <- length(wsres[[i]]$objects[,1])
        }
        else {
            if (!is.null(wsres$objects))
                nobj <- length(wsres$objects[,1])
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
    return(.CallEBImage("ws_paint", wsres, res, cols, fill, brds, opac))
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Returns images for objects determined by ws function - a list of image stacks
# with one object per image
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
wsImages <- function(wsres, ref) {
    if(!assert(ref))
        stop("wrong class of argument ref")
    return(.CallEBImage("ws_images", wsres, ref))
}

