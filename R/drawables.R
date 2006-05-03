# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.draw <- function(x, drawable, modify = TRUE) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    if (!is(drawable, "Drawable"))
        stop("Argument drawable must be one of descendants of class Drawable")
    if (!modify) {
        x = copy(x)
        return(.CallEBImage("drawShapes", x, drawable))
    }
    else # original data modified
        invisible(.CallEBImage("drawShapes", x, drawable))
}

draw <- function(x, drawable) {
    if (missing(x) || missing(drawable))
        stop("Arguments missing")
    .draw(x, drawable, modify = FALSE)
}
