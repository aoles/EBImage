# -------------------------------------------------------------------------
# Morphological filters: erode
 
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

.erode <- function(x, kernel=matrix(TRUE,3,3), iter=2) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    return(.CallEBImage("morph_erode", x, as.integer(kernel), as.integer(iter)))
}
