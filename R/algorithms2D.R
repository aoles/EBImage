# ============================================================================
# Algorithms of image analysis
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
objectCount <- function(x, ref = NULL, minArea = 20, maxRadius = 100, tolerance = 1, maxObjects = 1000, modify = FALSE) {
    .notImageError(x)
    if (!is.null(ref)) {
        .notImageError(ref)
        # FIXME : ensure here that both images have the same size
    }
    if (x@rgb)
        stop("objectCount function can be applied to Distance Maps only, which must be grayscales")
    if (!is.null(ref))
        if (ref@rgb)
            stop("objectCount can count intensity of grayscale reference images only")
    if (!modify)
        x = copyImage(x)
    param = c(minArea, maxRadius, tolerance, maxObjects)
    return(.CallEBImage("objectCount", x, ref, as.double(param)))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#paintObjectCountMap <- function(x) {
#    .notImageError(x)
#    y = toRGB(-x)
#    nlevels = range(-x)[[2]]
#    for (i in 1:nlevels) {
#        cols = as.integer(runif(3,100,255))
#        color = cols[[1]] + cols[[2]] * 255 + cols[[3]] * 65535
#        y[x == - i] = color
#    }
#    return(y)
#}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
