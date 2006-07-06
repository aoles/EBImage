# -------------------------------------------------------------------------
# Morphological filters: mErode, mDilate, mOpen, mClose, mKernel
 
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

mKernel <- function(size = 5, shape = "round") {
    if (size < 3 || (size / 2 == as.integer(size / 2)))
        stop("kernel size must be an odd number >= 3: [3, 5, 7, ...")
    if(switch(shape, round=, square=FALSE, TRUE))
        stop("available shapes 'round' and 'square'")
    res <- matrix(as.integer(FALSE), size, size, byrow = TRUE)
    cx = as.integer(size / 2) + 1
    if (shape == "round") {
        res[cx,] = as.integer(TRUE)
        res[,cx] = as.integer(TRUE)
        for (i in 1:(cx-1))
            for (j in 1:(cx-1))
                if ((cx - i)^2 + (cx - j)^2 <= (cx - 1)^2) {
                    res[i, j] = as.integer(TRUE)
                    res[size - i + 1, j] = as.integer(TRUE)
                    res[i, size - j + 1] = as.integer(TRUE)
                    res[size - i + 1, size - j + 1] = as.integer(TRUE)
                }
        return(res)    
    }

    # otherwise square
    res[] = as.integer(TRUE)
    return(res)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
.mErode <- function(x, iter = 1, kernel = mKernel(5), modify = TRUE) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    if (x@rgb)
        stop("Only binary images are supported at the moment: {0},{1}")
    if (!is.integer(kernel) || !is.matrix(kernel))
        stop("kernel must be an integer matrix")
    if (!modify) {
        x <- copy(x)
        # 0 - erode, 1 - dilate
        return(.CallEBImage("erodeDilate", x, kernel, as.integer(iter), as.integer(0))) 
    }
    else # original data modified
        invisible(.CallEBImage("erodeDilate", x, kernel, as.integer(iter), as.integer(0))) 
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mErode <- function(x, iter = 1, kernel = mKernel(5)) {
    .mErode(x, kernel, iter, FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
.mDilate <- function(x, iter = 1, kernel = mKernel(5), modify = TRUE) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    if (x@rgb)
        stop("Only binary images are supported at the moment: {0},{1}")
    if (!is.integer(kernel) || !is.matrix(kernel))
        stop("kernel must be an integer matrix")
    if (!modify) {
        x <- copy(x)
        # 0 - erode, 1 - dilate
        return(.CallEBImage("erodeDilate", x, kernel, as.integer(iter), as.integer(1))) 
    }
    else # original data modified
        invisible(.CallEBImage("erodeDilate", x, kernel, as.integer(iter), as.integer(1))) 
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mDilate <- function(x, iter = 1, kernel = mKernel(5)) {
    .mDilate(x, kernel, iter, FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
.mOpen <- function(x, iter = 1, kernel = mKernel(5), modify = TRUE) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    if (x@rgb)
        stop("Only binary images are supported at the moment: {0},{1}")
    if (!modify)
        x <- copy(x)
    if (modify)
        invisible(.mDilate(.mErode(x, kernel, iter, TRUE), kernel, iter, TRUE))
    else
        return(.mDilate(.mErode(x, kernel, iter, TRUE), kernel, iter, TRUE))
}    
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mOpen <- function(x, iter = 1, kernel = mKernel(5)) {
    .mOpen(x, kernel, iter, FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
.mClose <- function(x, iter = 1, kernel = mKernel(5), modify = TRUE) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    if (x@rgb)
        stop("Only binary images are supported at the moment: {0},{1}")
    if (!modify)
        x <- copy(x)
    if (modify)
        invisible(.mErode(.mDilate(x, kernel, iter, TRUE), kernel, iter, TRUE))
    else
        return(.mErode(.mDilate(x, kernel, iter, TRUE), kernel, iter, TRUE))
}    
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mClose <- function(x, iter = 1, kernel = mKernel(5)) {
    .mClose(x, kernel, iter, FALSE)
}
