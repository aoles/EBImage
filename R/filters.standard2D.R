# ============================================================================
# 2D Image Processing routines based on Magick++ functions
# 1-to-1 R implementations of Magick::Image methods in stdFilters.cpp
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
# IMAGE PROCESSING ROUTINES via ImageMagick
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
tresh <- function(x, width = 20, height = 20, offset = 1000, preprocess = FALSE) {
    .notImageError(x)
    param = as.double(c(width, height, offset))
    filter = as.integer(1)
    if (preprocess)
        x = gaussFilter(normalize(x), 4, 2)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
blur <- function(x, radius = 1, sigma = 0.5) {
    .notImageError(x)
    param = as.double(c(radius, sigma))
    filter = as.integer(2)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
contrast <- function(x, sharpen) {
    .notImageError(x)
    if (missing(sharpen))
        stop("argument 'sharpen' is essential")
    param = as.double(sharpen)
    filter = as.integer(3)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
despeckle <- function(x) {
    .notImageError(x)
    filter = as.integer(4)
    return(.CallEBImage("stdFilter2D", x, filter, NULL))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
edge <- function(x, radius = 1) {
    .notImageError(x)
    param = as.double(radius)
    filter = as.integer(5)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
enhance <- function(x) {
    .notImageError(x)
    filter = as.integer(6)
    return(.CallEBImage("stdFilter2D", x, filter, NULL))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
equalize <- function(x) {
    .notImageError(x)
    filter = as.integer(7)
    return(.CallEBImage("stdFilter2D", x, filter, NULL))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ffill <- function(x, col, row, color) {
    .notImageError(x)
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
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ffillEdge <- function(x, col, row, color) {
    .notImageError(x)
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
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
colorGamma <- function(x, level) {
    .notImageError(x)
    if (missing(level))
        stop("argument 'level' is essential")
    param = as.double(level)
    filter = as.integer(10)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
gaussFilter <- function(x, width = 1, sigma = 0.5) {
    .notImageError(x)
    param = as.double(c(width, sigma))
    filter = as.integer(11)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
medianFilter <- function(x, radius = 2) {
    .notImageError(x)
    param = as.double(radius)
    filter = as.integer(12)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mod <- function(x, brightness = 1, saturation = 1, hue = 1) {
    .notImageError(x)
    param = as.double(c(brightness, saturation, hue))
    filter = as.integer(13)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
redNoise <- function(x, order = NULL) {
    .notImageError(x)
    if (is.null(order))
        order = -1
    param = as.double(order)
    filter = as.integer(14)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rotate <- function(x, degrees = 90) {
    .notImageError(x)
    param = as.double(degrees)
    filter = as.integer(15)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sampleImage <- function(x, dx, dy) {
    .notImageError(x)
    if (missing(dx) || missing(dy))
        stop("arguments 'dx' and 'dy' are essential")
    param = as.double(c(dx, dy))
    filter = as.integer(16)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
scaleImage <- function(x, dx, dy) {
    .notImageError(x)
    if (missing(dx) || missing(dy))
        stop("arguments 'dx' and 'dy' are essential")
    param = as.double(c(dx, dy))
    filter = as.integer(17)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
segment <- function(x, cluster = 1, smooth = 1.5) {
    .notImageError(x)
    param = as.double(c(cluster, smooth))
    filter = as.integer(18)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shade <- function(x, azimuth = 30, elevation = 30, shading = FALSE) {
    .notImageError(x)
    param = as.double(c(azimuth, elevation))
    filter = as.integer(19)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sharpen <- function(x, radius = 1, sigma = 0.5) {
    .notImageError(x)
    param = as.double(c(radius, sigma))
    filter = as.integer(20)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
solarize <- function(x, factor = 50) {
    .notImageError(x)
    param = as.double(factor)
    filter = as.integer(21)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
spread <- function(x, amount = 3) {
    .notImageError(x)
    param = as.double(amount)
    filter = as.integer(22)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unsharpMask <- function(x, radius = 2, sigma = 0.5, amount = 5, threshold = 2) {
    .notImageError(x)
    param = as.double(c(radius, sigma, amount, threshold))
    filter = as.integer(23)
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# possible types: uniform, gaussian, multi(plicativeGaussian), impulse, laplace(ian), poisson
noise <- function(x, type = "gaussian") {
    .notImageError(x)
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
    return(.CallEBImage("stdFilter2D", x, filter, param))
}
