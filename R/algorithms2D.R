# ============================================================================
# Algorithms of image analysis
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005-2006
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================

# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.objectCount <- function(x, ref = NULL, minArea = 20, maxRadius = 100, tolerance = 1, maxObjects = 1000, modify = TRUE) {
    .notImageError(x)
    if (!is.null(ref)) {
        .notImageError(ref)
        # FIXME : ensure here that both images have the same size
    }
    if (x@rgb)
        stop("Function supports distance maps only, which must be grayscale")
    if (!is.null(ref))
        if (ref@rgb)
            stop("Reference image must be grayscale")
    if (!modify)
        x = copyImage(x)
    param = c(minArea, maxRadius, tolerance, maxObjects)
    return(.CallEBImage("objectCount", x, ref, as.double(param)))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
objectCount <- function(x, ref = NULL, minArea = 20, maxRadius = 100, tolerance = 1, maxObjects = 1000) {
    .objectCount(x, ref, minArea, maxRadius, tolerance, maxObjects, modify = FALSE)
}