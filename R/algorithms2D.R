# ============================================================================
# Algorithms of image analysis
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
# TODO: add more algorithms in distmaps.cpp if necessary
distMap <- function(x, alg = "Lotufo_Zampirolli", modify = FALSE) {
    .notImageError(x)
    if (x@rgb)
        stop("cannot apply Distance Map algorithm onto RGB images")
    ialg = as.integer(grep(alg, c("Lotufo_Zampirolli")))
    if(length(ialg)==0)
      stop(sprintf("Invalid algorithm 'alg'=%s.", alg))
    if(length(ialg)>1)
      stop(sprintf("Specification of algorithm 'alg'=%s is ambiguous.", alg))

    # 060105-01 TODO
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("distMap", x, ialg))
    }
    else
        invisible(.CallEBImage("distMap", x, ialg))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
# This function migrated to Biobase as matchpt!
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# nn <- function(x) {
#    if (is.vector(x))
#        x <- matrix(x, ncol = 1, nrow = length(x))
#    if (!is.matrix(x))
#        stop("x must be a matrix in call to nn (nearest neighbour)")
#    dims <- dim(x)
#    if (length(dims) != 2)
#        stop("wrong argument dimensions")
#    return(.CallEBImage("nn", x))
# }