# -------------------------------------------------------------------------
# Package initialization code
 
# Copyright (c) 2006 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License 
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.          

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

# -------------------------------------------------------------------------
.CallEBImage <- function(name, ...) {
    .Call(name, ..., PACKAGE = "EBImage")
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
verboseEBImage <- function(verbose = TRUE) {
    .CallEBImage("setVerbose", as.logical(verbose))
    invisible(FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.onLoad <- function(lib, pkg) {
    ## use useDynLib("EBImage") in NAMESPACE instead
    ## library.dynam("EBImage", pkg, lib)
    require("methods")
    verboseEBImage(FALSE)
    cat("  .: EBImage of Bioconductor.org :.\n")
}
