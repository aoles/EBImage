# -------------------------------------------------------------------------
# Adaptive threshold filter or images
# Bundled with src/flt_thresh.{h,cpp}
 
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
