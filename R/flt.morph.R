# -------------------------------------------------------------------------
# Morphological filters: erode, dilate, mopen, mclose
 
# Copyright (c) 2006 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License 
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.          

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

# See the GNU General Public License for more details.
# GPL license wording: http://www.gnu.org/licenses/gpl.html

# -------------------------------------------------------------------------
.erode <- function(x, kernel = matrix(TRUE,3,3), iter = 2, modify = TRUE) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    if (x@rgb)
        stop("Only grayscale images are supported (only binary at the moment: {0,1})")
    if (!modify) {
        x <- copy(x)
        # 0 - erode, 1 - dilate
        return(.CallEBImage("erodeDilate", x, as.logical(kernel), as.integer(iter), as.integer(0))) 
    }
    else # original data modified
        invisible(.CallEBImage("erodeDilate", x, as.logical(kernel), as.integer(iter), as.integer(0))) 
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
erode <- function(x, kernel = matrix(TRUE,3,3), iter = 2) {
    return(.erode(x, kernel, iter, FALSE))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
.dilate <- function(x, kernel = matrix(TRUE,3,3), iter = 2, modify = TRUE) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    if (x@rgb)
        stop("Only grayscale images are supported (only binary at the moment: {0,1})")
    if (!modify) {
        x <- copy(x)
        # 0 - erode, 1 - dilate
        return(.CallEBImage("erodeDilate", x, as.logical(kernel), as.integer(iter), as.integer(1))) 
    }
    else # original data modified
        invisible(.CallEBImage("erodeDilate", x, as.logical(kernel), as.integer(iter), as.integer(1))) 
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
dilate <- function(x, kernel = matrix(TRUE,3,3), iter = 2) {
    return(.dilate(x, kernel, iter, FALSE))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
.mopen <- function(x, kernel = matrix(TRUE,3,3), iter = 2, modify = TRUE) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    if (x@rgb)
        stop("Only grayscale images are supported (only binary at the moment: {0,1})")
    if (!modify)
        x <- copy(x)
    .erode(x, kernel, iter, TRUE)
    if (modify)
        invisible(.dilate(x, kernel, iter, TRUE))
    else
        return(.dilate(x, kernel, iter, TRUE))
}    
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mopen <- function(x, kernel = matrix(TRUE,3,3), iter = 2) {
    return(.mopen(x, kernel, iter, FALSE))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
.mclose <- function(x, kernel = matrix(TRUE,3,3), iter = 2, modify = TRUE) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    if (x@rgb)
        stop("Only grayscale images are supported (only binary at the moment: {0,1})")
    if (!modify)
        x <- copy(x)
    .dilate(x, kernel, iter, TRUE)
    if (modify)
        invisible(.erode(x, kernel, iter, TRUE))
    else
        return(.erode(x, kernel, iter, TRUE))
}    
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mclose <- function(x, kernel = matrix(TRUE,3,3), iter = 2) {
    return(.mclose(x, kernel, iter, FALSE))
}
