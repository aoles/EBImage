# -------------------------------------------------------------------------
# Distance map filter for images
# Bundled with src/flt_distmap.{h,cpp}
 
# Copyright (c) 2005 Oleg Sklyar

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
