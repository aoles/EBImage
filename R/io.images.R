# ============================================================================
# IO routines for image classes (Image2D, Image3D)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
setGeneric("write.image", function(object, files) standardGeneric("write.image"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("write.image", signature(object = "Image2D", files = "character"),
    function(object, files) {
        files = as.character(files)
        if (class(object) == "Image2D" && length(files) != 1)
            stop("single file/URL name must be supplied for images of class Image2D")
        if (class(object) == "Image3D" && length(files) != 1 && length(files) != dim(object)[[3]])
            stop("number of files/URL must match the stack size for a 3D images or a single file supporting stacks (TIFF) must be supplied")
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
        stop("at least one file/URL must be supplied")
#    error = file.exists(files)
#    if(length(error[error == FALSE]) > 0) {
#        cat("the following files were not found: ")
#        cat(paste(files[error == FALSE], " ", sep = ''))
#        cat("\n")
#        stop("file(s) not found")
#    }
    rgb = as.logical(rgb)[[1]]
    return(.CallEBImage("readImages", files, rgb))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ping.image <- function(files, show.comments = FALSE) {
    files = as.character(files)
    if (length(files) < 1)
        stop("at least one file/URL must be supplied")
    error = TRUE #file.exists(files)
    if(length(error[error == FALSE]) > 0) {
        cat("the following files were not found: ")
        cat(paste(files[error == FALSE], " ", sep = ''))
        cat("\n")
        stop("file(s) not found")
    }
    invisible(.CallEBImage("pingImages", files, as.logical(show.comments)))
}

