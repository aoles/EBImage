.First.lib <- function(lib, pkg) {
  library.dynam("ebimage", pkg, lib)
  cat("\nWelcome to 'EBImage' - a part of 'Bioconductor.org' project\n\n")
  cat("\tType 'help(EBImage)' to get started, or use\n")
  cat("\t'openVignette()' from 'Biobase' to read the manual\n\n")
}

.CallImagine <- function(name, ...) {
    .Call(name, ..., PACKAGE = "imagine")
}
