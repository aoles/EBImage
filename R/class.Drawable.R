# -------------------------------------------------------------------------
# Classes around Drawable, definitions
 
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
setClass("Drawable",
    representation(
        x           = "matrix",
        strokeColor = "character",
        strokeWidth = "numeric",
        fillColor   = "character",
        doFill      = "logical",
        fillOpacity = "numeric"
    ),
    prototype(
        x           = matrix(0, 4, 1),
        strokeColor = "#FFFFFF",
        strokeWidth = 1,
        fillColor   = "#000000",
        doFill      = TRUE,
        fillOpacity = 0.5
    )
)
# ============================================================================
# setClass("DrawableText",
#     contains = "Drawable"
# )
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# DrawableText <- function(text, x = 0, y = 0) {
#     error("to be implemented")
# }
# ============================================================================
setClass("DrawableCircle",
    contains = "Drawable",
    prototype(
        x           = matrix(0, ncol = 3, nrow = 1)
    ),
    validity = function(object) {
        return(dim(object@x)[[2]] == 3)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DrawableCircle <- function(x, y, r) {
    if (missing(x) || missing(y) || missing(r))
        stop("Please supply x-y centre coordinates and radius")
    if (length(x) != length(y) || length(x) != length(r))
        stop("Vectors of coordinates and radius must be of the same length")
    return(new("DrawableCircle", x = matrix(c(x, y, r), ncol = 3, nrow = length(x))))
}
# ============================================================================
setClass("DrawableLine",
    contains = "Drawable",
    prototype(
        x           = matrix(0, ncol = 4, nrow = 1)
    ),
    validity = function(object) {
        return(dim(object@x)[[2]] == 4)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DrawableLine <- function(x1, y1, x2, y2) {
    if (missing(x1) || missing(y1) || missing(x2) || missing(y2))
        stop("Please supply all four coordinates")
    if (length(x1) != length(y1) || length(x2) != length(y2) || length(x1) != length(x2))
        stop("Vectors of coordinates must be of the same length")
    return(new("DrawableLine", x = matrix(c(x1, y1, x2, y2), ncol = 4, nrow = length(x1))))
}
# ============================================================================
setClass("DrawableRect",
    contains = "DrawableLine"
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DrawableRect <-function(x1, y1, x2, y2) {
    if (missing(x1) || missing(y1) || missing(x2) || missing(y2))
        stop("Please supply all four coordinates")
    if (length(x1) != length(y1) || length(x2) != length(y2) || length(x1) != length(x2))
        stop("Vectors of coordinates must be of the same length")
    return(new("DrawableRect", x = matrix(c(x1, y1, x2, y2), ncol = 4, nrow = length(x1))))
}
# ============================================================================
setClass("DrawableEllipse",
    contains = "Drawable",
    prototype(
        x           = matrix(0, ncol = 6, nrow = 1)
    ),
    validity = function(object) {
        return(dim(object@x)[[2]] == 6)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DrawableEllipse <-function(x1, y1, x2, y2, sgrad = 0, egrad = 360) {
    if (missing(x1) || missing(y1) || missing(x2) || missing(y2))
        stop("Please supply all four coordinates")
    if (length(x1) != length(y1) || length(x2) != length(y2) || length(x1) != length(x2))
        stop("Vectors of coordinates must be of the same length")
    if (length(sgrad) != length(x1) || length(egrad) != length(x1)) {
        .sgrad = sgrad[[1]]
        .egrad = egrad[[1]]
        sgrad = numeric(length(x1))
        sgrad[] = .sgrad
        egrad = numeric(length(x1))
        egrad[] = egrad[[1]]
    }
    return(new("DrawableEllipse", x = matrix(c(x1, y1, x2, y2, sgrad, egrad), ncol = 6, nrow = length(x1))))
}
