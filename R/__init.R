# Package initialization routine
.onLoad <- function(lib, pkg) {
    # uses useDynLib("EBImage") in NAMESPACE instead
    #library.dynam("EBImage", pkg, lib)
    cat("* EBImage of Bioconductor.org: help(EBImage) to get started...\n")
    require(methods)
}

# shortcut to call library functions for EBImage
.CallEBImage <- function(name, ...) {
    .Call(name, ..., PACKAGE = "EBImage")
}
