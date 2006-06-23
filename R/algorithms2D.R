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
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.watershed <- function(x, ref = NULL, mindist = 15, minradius = 10, rm.edges = TRUE, modify = TRUE) {
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
    param = c(mindist, minradius, as.double(rm.edges))
    res <- .CallEBImage("watershedDetection", x, ref, NULL, as.double(param))
    if (!is.null(res)) {
        colnames(res) <- c("index", "x", "y", "intens", "size", "perim", "edge") #, "dx", "dy")
        if (rm.edges) res <- res[res[,7] == 0,]
    }
    return(res)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
watershed <- function(x, ref = NULL, mindist = 15, minradius = 10, rm.edges = TRUE) {
    .watershed(x, ref, mindist, minradius, rm.edges, FALSE)
}
