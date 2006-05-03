setClass("Drawable",
    representation(
        x           = "matrix",
        strokeColor = "character",
        strokeWidth = "numeric",
        fillColor   = "character"
    ),
    prototype(
        x           = matrix(0, 4, 1),
        strokeColor = "#FFFFFF",
        strokeWidth = 1,
        fillColor   = "-"
    )
)

setClass("DrawableText",
    contains = "Drawable"
)

DrawableText <- function(text, x = 0, y = 0) {
    error("to be implemented")
}

setClass("DrawableCircle",
    contains = "Drawable",
    prototype(
        x           = matrix(0, 3, 1)
    ),
    validity = function(obj) {
        return(dim(obj@x)[[2]] == 3)
    }
)

DrawableCircle <- function(x, y, r) {
    if (missing(x) || missing(y) || missing(r))
        stop("Please supply x-y centre coordinates and radius")
    if (length(x) != length(y) || length(x) != length(r))
        stop("Vectors of coordinates and radius must be of the same length")
    return(new("DrawableCircle", x = matrix(c(x, y, r), 3, length(x))))
}

setClass("DrawableLine",
    contains = "Drawable",
    validity = function(obj) {
        return(dim(obj@x)[[2]] == 4)
    }
)

DrawableLine <- function(x1, y1, x2, y2) {
    if (missing(x) || missing(y) || missing(x1) || missing(y1))
        stop("Please supply all four coordinates")
    if (length(x1) != length(y1) || length(x2) != length(y2) || length(x1) != length(x2))
        stop("Vectors of coordinates must be of the same length")
    return(new("DrawableLine", x = matrix(c(x1, y1, x2, y2), 4, length(x1))))
}

setClass("DrawableRect",
    contains = "DrawableLine"
)

DrawableRect <-function(x1, y1, x2, y2) {
    if (missing(x) || missing(y) || missing(x1) || missing(y1))
        stop("Please supply all four coordinates")
    if (length(x1) != length(y1) || length(x2) != length(y2) || length(x1) != length(x2))
        stop("Vectors of coordinates must be of the same length")
    return(new("DrawableRect", x = matrix(c(x1, y1, x2, y2), 4, length(x1))))
}

setClass("DrawableEllipse",
    contains = "DrawableLine"
)

DrawableEllipse <-function(x1, y1, x2, y2) {
    if (missing(x) || missing(y) || missing(x1) || missing(y1))
        stop("Please supply all four coordinates")
    if (length(x1) != length(y1) || length(x2) != length(y2) || length(x1) != length(x2))
        stop("Vectors of coordinates must be of the same length")
    return(new("DrawableEllipse", x = matrix(c(x1, y1, x2, y2), 4, length(x1))))
}
