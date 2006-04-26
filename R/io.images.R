# ============================================================================
# IO routines for image classes (Image2D, Image3D)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
setGeneric("write.image", function(object, files) standardGeneric("write.image"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("write.image", signature(object = "Image", files = "character"),
    function(object, files) {
        files = as.character(files)
        if (!isCorrectType(object))
            object = correctType(object)
        invisible(.CallEBImage("writeImages", object, files))
    }
)
# ============================================================================
# ASSOCIATED ROUTINES
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read.image <- function(files, rgb = FALSE) {
    files = as.character(files)
    if (length(files) < 1)
        stop("At least one file/URL must be supplied")
    rgb = as.logical(rgb)[[1]]
    return(.CallEBImage("readImages", files, rgb))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ping.image <- function(files, show.comments = FALSE) {
    files = as.character(files)
    if (length(files) < 1)
        stop("At least one file/URL must be supplied")
    invisible(.CallEBImage("pingImages", files, as.logical(show.comments)))
}

