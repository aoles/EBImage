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
objectCount <- function(x, minArea = 20, maxRadius = 100, tolerance = 1, maxObjects = 1000) {
    .notImageError(x)
    if (x@rgb)
        stop("objectCount function can be applied to Distance Maps only, which must be grayscales")
    if (!is.double(x))
        x = as.double(x)
    param = c(minArea, maxRadius, tolerance, maxObjects)
    return(.CallEBImage("objectCount", x, as.double(param)))
}
