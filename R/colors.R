# -------------------------------------------------------------------------
# Operations with colors: conversions for different vectors: ANY <--> ANY
# Bundled with src/colors.{h,cpp}
 
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

# Assumed: numeric - gray; integer - RGB; character - X11 string

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toGray", signature(object = "ANY"),
    function(object) {
        return(.CallEBImage("any2gray", object))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toRGB", signature(object = "ANY"),
    function(object) {
        return(.CallEBImage("any2rgb", object))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toX11char", signature(object = "ANY"),
    function(object) {
        return(.CallEBImage("any2X11char", object))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("add2RGB", signature(x = "ANY", y = "ANY"),
    function(x, y) {
        return(.CallEBImage("add2rgb", x, y))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("sub2RGB", signature(x = "ANY", y = "ANY"),
    function(x, y) {
        return(.CallEBImage("sub2rgb", x, y))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("scale2RGB", signature(x = "ANY", mult = "numeric"),
    function(x, mult) {
        return(.CallEBImage("scale2rgb", x, mult))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toRed", signature(object = "ANY"),
    function(object) {
        return(.CallEBImage("asred", object))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toGreen", signature(object = "ANY"),
    function(object) {
        return(.CallEBImage("asgreen", object))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toBlue", signature(object = "ANY"),
    function(object) {
        return(.CallEBImage("asblue", object))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getRed", signature(object = "ANY"),
    function(object) {
        return(.CallEBImage("getred", object))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getGreen", signature(object = "ANY"),
    function(object) {
        return(.CallEBImage("getgreen", object))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getBlue", signature(object = "ANY"),
    function(object) {
        return(.CallEBImage("getblue", object))
    }
)
