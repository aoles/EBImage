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
paintWatershed2D <- function(x, img, col = c("blue", "white", "red"), do.fill = TRUE, do.borders = TRUE) {
    if(!assert(img))
        stop("Wrong class of argument img")
    if (dim(img)[[3]] > 1)
        stop("function implemented for single 2D images only, use subscripts");
    if (!do.fill && !do.borders)
        stop("both do.fill and do.borders set to FALSE - nothing to do")
    if (!img@rgb)
        stop("img must be RGB, convert to RGB first")
    if (is.null(x$objects))
        stop("x must contain 'objects' element as returned by watershed function")   
    if (is.null(x$pixels) && do.fill)
        stop("x must contain 'pixels' element to enable fill");
    if (is.null(x$borders) && do.borders)
        stop("x must contain 'borders' element to enable plotting borders");
    nobj <- dim(x$objects)[[1]]
    colRamp <- colorRamp(fcol)
    mcol = colRamp(((1:nobj) - 1) / nobj) / 256
    col = rgb(mcol[,1], mcol[,2], mcol[,3])
    
    
}
