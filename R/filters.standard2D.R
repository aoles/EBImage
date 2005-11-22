# ============================================================================
# 2D Image Processing routines based on Magick++ functions
# 1-to-1 R implementations of Magick::Image methods in stdFilters.cpp
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
# Help functions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.warnOnStack <- function(x, do.stack = FALSE, warn.stack = TRUE, ...) {
    .notImageError(x)
    .dim = length(dim(x))
    if (.dim != 2) {
        if (.dim != 3)
            stop("the used set of image processing routines operates only on 2D and 3D images")
        if (!do.stack)
            stop("3D image supplied with 'do.stack = FALSE'. stopping to prevent undesired lengthy operation")
        if (warn.stack)
            warning("3D images will be processed as stacks - as one-by-one 2D")
    }
    invisible(TRUE)
}
# ============================================================================
# IMAGE PROCESSING ROUTINES via ImageMagick
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.tresh <- function(x, width = 20, height = 20, offset = 1000, ...) {
    .warnOnStack(x, ...)
    if (missing(width) || missing(height))
        stop("arguments 'width' and 'height' are essential")
    param = as.double(c(width, height, offset))
    filter = as.integer(1)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.smartTresh <- function(x, width = 20, height = 20, offset = 1000, ...) {
    .warnOnStack(x, ...)
    if (x@rgb)
        x = to.gray(x, ...)
    # normalize, smooth and segment
    x = im.tresh(im.gaussian(normalize(x), 4, 2, ...), width = width, height = height, offset = offset, ...)
    return(x)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.blur <- function(x, radius = 1, sigma = 0.5, ...) {
    .warnOnStack(x, ...)
    param = as.double(c(radius, sigma))
    filter = as.integer(2)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.contrast <- function(x, sharpen, ...) {
    .warnOnStack(x, ...)
    if (missing(sharpen))
        stop("argument 'sharpen' is essential")
    param = as.double(sharpen)
    filter = as.integer(3)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.despeckle <- function(x, ...) {
    .warnOnStack(x, ...)
    filter = as.integer(4)
    return(.CallImagine("stdFilter2D", x, filter, NULL))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.edge <- function(x, radius = 0, ...) {
    .warnOnStack(x, ...)
    param = as.double(radius)
    filter = as.integer(5)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.enhance <- function(x, ...) {
    .warnOnStack(x, ...)
    filter = as.integer(6)
    return(.CallImagine("stdFilter2D", x, filter, NULL))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.equalize <- function(x, ...) {
    .warnOnStack(x, ...)
    filter = as.integer(7)
    return(.CallImagine("stdFilter2D", x, filter, NULL))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.fill <- function(x, col, row, color, ...) {
    .warnOnStack(x, ...)
    if (missing(col) || missing(height))
        stop("arguments 'col', 'row' and 'color' are essential")
    if (x@rgb) {
        red = get.red(as.integer(color))
        green = get.green(as.integer(color))
        blue = get.blue(as.integer(color))
        param = as.double(c(col, row, red, green, blue))
    }
    else
        param = as.double(c(col, color))
    filter = as.integer(8)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.fillEdge <- function(x, col, row, color, ...) {
    .warnOnStack(x, ...)
    if (missing(col) || missing(height))
        stop("arguments 'col', 'row' and 'color' are essential")
    if (x@rgb) {
        red = get.red(as.integer(color))
        green = get.green(as.integer(color))
        blue = get.blue(as.integer(color))
        param = as.double(c(col, row, red, green, blue))
    }
    else
        param = as.double(c(col, color))
    filter = as.integer(9)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.gamma <- function(x, level, ...) {
    .warnOnStack(x, ...)
    if (missing(level))
        stop("argument 'level' is essential")
    param = as.double(level)
    filter = as.integer(10)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.gaussian <- function(x, width = 1, sigma = 0.5, ...) {
    .warnOnStack(x, ...)
    param = as.double(c(width, sigma))
    filter = as.integer(11)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.median <- function(x, radius = 2, ...) {
    .warnOnStack(x, ...)
    param = as.double(radius)
    filter = as.integer(12)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.mod <- function(x, brightness = 1, saturation = 1, hue = 1, ...) {
    .warnOnStack(x, ...)
    param = as.double(c(brightness, saturation, hue))
    filter = as.integer(13)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.redNoise <- function(x, order = NULL, ...) {
    .warnOnStack(x, ...)
    if (is.null(order))
        order = -1
    param = as.double(order)
    filter = as.integer(14)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.rotate <- function(x, degrees = 90, ...) {
    .warnOnStack(x, ...)
    param = as.double(degrees)
    filter = as.integer(15)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.sample <- function(x, dx, dy, ...) {
    .warnOnStack(x, ...)
    if (missing(dx) || missing(dy))
        stop("arguments 'dx' and 'dy' are essential")
    param = as.double(c(dx, dy))
    filter = as.integer(16)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.scale <- function(x, dx, dy, ...) {
    .warnOnStack(x, ...)
    if (missing(dx) || missing(dy))
        stop("arguments 'dx' and 'dy' are essential")
    param = as.double(c(dx, dy))
    filter = as.integer(17)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.segment <- function(x, cluster = 1, smooth = 1.5, ...) {
    .warnOnStack(x, ...)
    param = as.double(c(cluster, smooth))
    filter = as.integer(18)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.shade <- function(x, azimuth = 30, elevation = 30, shading = FALSE, ...) {
    .warnOnStack(x, ...)
    param = as.double(c(azimuth, elevation))
    filter = as.integer(19)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.sharpen <- function(x, radius = 1, sigma = 0.5, ...) {
    .warnOnStack(x, ...)
    param = as.double(c(radius, sigma))
    filter = as.integer(20)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.solarize <- function(x, factor = 50, ...) {
    .warnOnStack(x, ...)
    param = as.double(factor)
    filter = as.integer(21)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.spread <- function(x, amount = 3, ...) {
    .warnOnStack(x, ...)
    param = as.double(amount)
    filter = as.integer(22)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
im.unsharpmask <- function(x, radius = 2, sigma = 0.5, amount = 5, threshold = 2, ...) {
    .warnOnStack(x, ...)
    param = as.double(c(radius, sigma, amount, threshold))
    filter = as.integer(23)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# possible types: uniform, gaussian, multi(plicativeGaussian), impulse, laplace(ian), poisson
im.addNoise <- function(x, type = "gaussian", ...) {
    .warnOnStack(x, ...)
    param = as.double(
        switch(type,
            "uniform" = 1,
            "gaussian" = 2,
            "multi" = 3,
            "impulse" = 4,
            "laplace" = 5,
            "poisson" = 6,
            2
        )
    )
    filter = as.integer(24)
    return(.CallImagine("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# TODO: add more algorithms in distmaps.cpp if necessary
im.distMap <- function(x, alg = "Lotufo_Zampirolli", ...) {
    .warnOnStack(x, ...)
    # res value will be modified in call to distMap
    if (is.integer(x))
        res = x
    else
        res = as.integer(x)
    res@rgb = FALSE
    ialg = as.integer(grep(alg, c("Lotufo_Zampirolli")))
    if(length(ialg)==0)
      stop(sprintf("Invalid algorithm 'alg'=%s.", alg))
    if(length(ialg)>1)
      stop(sprintf("Specification of algorithm 'alg'=%s is ambiguous.", alg))

    return(.CallImagine("distMap", res, ialg))
}
