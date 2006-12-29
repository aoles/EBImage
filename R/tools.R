# Package tools

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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.DoCall <- function (name, ...) {
    .Call(name, ..., PACKAGE = "EBImage")
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
. <- function (string) {
    .DoCall ("lib_", as.character(string) )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("channel", signature(x="ANY", mode="character"),
    function (x, mode, ...) {
        mode <- tolower (mode)
        if ( mode == "grey" ) mode = "gray"
        modeNo <- switch (EXPR=mode, rgb=0, gray=1, red=2, green=3, 
                blue=4, asred=5, asgreen=6, asblue=7, x11=8, -1)
        if ( modeNo < 0 )
            stop ( paste(.("wrong conversion mode. Please specify one of"), "rgb, gray, grey, red, green, blue, asred, asgreen, asblue, x11") )
        if ( !is.numeric(x) && !is.integer(x) && !is.array(x) && 
             !is.matrix(x) && !is.character(x) )
             stop ( .("supported types are numeric, integer, character, array, and matrix" ) )
        if ( is.array(x) && !is.integer(x) && !is.numeric(x) )
            stop ( .("supported arrays must be either numeric or interger based") )
        res <- .DoCall("lib_channel", x, as.integer(modeNo) )
        if ( !is.null(res) )
            res [ which( is.na(x) ) ] = NA
        if ( is.null(res) || is.character(res) ) return (res)
        if ( is.array(x) ) dim (res) <- dim (x)
        return (res)
    }
)

