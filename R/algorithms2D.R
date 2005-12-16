# ============================================================================
# Algorithms of image analysis
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
# TODO: add more algorithms in distmaps.cpp if necessary
distMap <- function(x, alg = "Lotufo_Zampirolli") {
    .notImageError(x)
    # res value will be modified in call to distMap
    if (is.integer(x))
        res = x
    else
        res = as.integer(x)
    res@rgb = FALSE
    ialg = as.integer(grep(alg, c("Lotufo_Zampirolli")))
    if(length(ialg)==0)
      stop(sprintf("Invalid algorithm 'alg'=%s.", alg))
    if(length(ialg)>1)
      stop(sprintf("Specification of algorithm 'alg'=%s is ambiguous.", alg))

    return(.CallEBImage("distMap", res, ialg))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
objectCount <- function(x, minArea = 20, maxRadius = 100, tolerance = 3, maxObjects = 1000) {
    .notImageError(x)
    # res value will be modified in call to distMap
    if (is.integer(x))
        res = x
    else
        res = as.integer(x)
    res@rgb = FALSE
    param = c(minArea, maxRadius, tolerance, maxObjects)
    return(.CallEBImage("objectCount", res, as.double(param)))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
displayObjectCount <- function(x, objects) {
    .notImageError(x)
    if (is(x, "Image3D"))
        stop("This function is defined for Image2D only")
    res = toRGB(x)
    radii = sqrt(objects[2,] / 3.14159)
    res[objects[1,] - radii] = as.integer(65535)
    res[objects[1,] + radii] = as.integer(65535)
    res[objects[1,]] = as.integer(255)
    display(res)
}
