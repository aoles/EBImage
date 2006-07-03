# ============================================================================
# Algorithms of image analysis
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005-2006
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================

# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.objectCount <- function(x, ref = NULL, minArea = 20, maxRadius = 100, tolerance = 1, maxObjects = 1000, modify = TRUE) {
    if(!assert(x))
        stop("Wrong class of argument x")
    if (x@rgb)
        stop("Function supports distance maps only, which must be grayscale")
    if (!is.null(ref)) {
        if(!assert(ref))
            stop("Wrong class of argument ref")
        if (ref@rgb)
            stop("Reference image must be grayscale")
        if(!assert(x, ref))
            stop("Supplied images have different size or color scheme")
    }
    if (!modify)
        x = copy(x)
    param = c(minArea, maxRadius, tolerance, maxObjects)
    return(.CallEBImage("objectCount", x, ref, as.double(param)))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
objectCount <- function(x, ref = NULL, minArea = 20, maxRadius = 100, tolerance = 1, maxObjects = 1000) {
    .objectCount(x, ref, minArea, maxRadius, tolerance, maxObjects, modify = FALSE)
}

# seeds must be a 2D integer matrix cols - x,y, if more than 1 image - list of such

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.watershed <- function(x, mindist = 15, minradius = 10, edgeFactor = 0.2, seeds = NULL, ref = NULL, modify = TRUE) {
    if(!assert(x))
        stop("Wrong class of argument x")
    if (x@rgb)
        stop("Function supports distance maps only, which must be grayscale")
    if (!is.null(ref)) {
        if(!assert(ref))
            stop("Wrong class of argument ref")
        if (ref@rgb)
            stop("Function supports grayscale reference images only")
        .dimx <- dim(x)
        .dimref <- dim(ref)
        if (.dimx[[1]] != .dimref[[1]] || .dimx[[2]] != .dimref[[2]] || .dimx[[3]] != .dimref[[3]])
            stop("distance map and reference images must be of the same size")
    }
    nimg <- dim(x)[[3]]
    if (!is.null(seeds)) {
        if (nimg == 1 && !is.matrix(seeds))
            stop("seeds must be a 2D matrix of integers for one single 2D image")
        if (nimg > 1 && !is.list(seeds))
            stop("seeds must be a list of 2D matrices of integers for a stack of images")
    }
    if (!modify)
        x = copy(x)
    param = c(mindist, minradius, edgeFactor)
    res <- .CallEBImage("watershedDetection", x, ref, seeds, as.double(param))
    return(res)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
watershed <- function(x, mindist = 15, minradius = 10, edgeFactor = 0.2, seeds = NULL, ref = NULL) {
    .watershed(x, mindist, minradius, edgeFactor, seeds, ref, FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
paintx <- function(x, img, col = "default", do.fill = TRUE, do.borders = TRUE, opacity = 0.2) {
    if(!assert(img))
        stop("Wrong class of argument img")
    if (col == "default")
        col <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
    res <- .copyHeader(img, rgb = TRUE);
    res@.Data <- array(scale2rgb(img@.Data, 1.0 - as.numeric(opacity)), dim(img))
    if (!do.fill && !do.borders)
        return(res)
    nimg <- dim(img)[[3]]
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
            r <- .CallEBImage("asRed", mcol[,1])
            g <- .CallEBImage("asGreen", mcol[,2])
            b <- .CallEBImage("asBlue", mcol[,3])
            cols[[i]] <- r + g + b
        } 
        else 
            cols[[i]] = NULL    
    }
    return(.CallEBImage("paintWatershed", x, res, cols, as.logical(do.fill), as.logical(do.borders), as.numeric(opacity)))
}
