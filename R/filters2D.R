# ============================================================================
# Image processing routines designed specifically for EBImage
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.distMap <- function(x, alg = "LotufoZampirolli", modify = TRUE) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    if (x@rgb)
        stop("cannot apply Distance Map algorithm onto RGB images")
    ialg = as.integer(grep(alg, c("EBImage", "LotufoZampirolli")))
    if(length(ialg)==0)
      stop(sprintf("Invalid algorithm 'alg'=%s.", alg))
    if(length(ialg)>2)
      stop(sprintf("Specification of algorithm 'alg'=%s is ambiguous.", alg))

    # 060105-01 TODO
    if (!modify) {
        x = copy(x)
        return(.CallEBImage("distMap", x, ialg))
    }
    else
        invisible(.CallEBImage("distMap", x, ialg))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
distMap <- function(x, alg = "LotufoZampirolli") {
    .distMap(x, alg, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.thresh <- function(x, width = 20, height = 20, offset = 0.05, preprocess = FALSE, modify = TRUE) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    param = as.double(c(width, height, offset))
    if (!modify) {
        if (preprocess)
            x = gaussFilter(normalize(x), 4, 2)
        else
            x = copy(x)
        return(.CallEBImage("adaptiveThreshold", x, param))
    }
    else { # original data modified
        if (preprocess) {
            .normalize(x, modify = TRUE)
            .gaussFilter(x, 4, 2, modify = TRUE)
        }
        invisible(.CallEBImage("adaptiveThreshold", x, param))
    }
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
thresh <- function(x, width = 20, height = 20, offset = 0.05, preprocess = FALSE) {
    .thresh(x, width, height, offset, preprocess, modify = FALSE)
}
