# ============================================================================
# Image processing routines designed specifically for EBImage
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
distMap <- function(x, alg = "EBImage", modify = FALSE) {
    .notImageError(x)
    if (x@rgb)
        stop("cannot apply Distance Map algorithm onto RGB images")
    ialg = as.integer(grep(alg, c("EBImage", "Lotufo_Zampirolli")))
    if(length(ialg)==0)
      stop(sprintf("Invalid algorithm 'alg'=%s.", alg))
    if(length(ialg)>2)
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
thresh <- function(x, width = 20, height = 20, offset = 0.05, preprocess = FALSE, modify = FALSE) {
    .notImageError(x)
    param = as.double(c(width, height, offset))
    if (!modify) {
        if (preprocess)
            x = gaussFilter(normalize(x), 4, 2)
        else
            x = copyImage(x)
        return(.CallEBImage("adaptiveThreshold", x, param))
    }
    else { # original data modified
        if (preprocess) {
            normalize(x, modify = TRUE)
            gaussFilter(x, 4, 2, modify = TRUE)
        }
        invisible(.CallEBImage("adaptiveThreshold", x, param))
    }
}
