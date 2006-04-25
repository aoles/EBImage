# ============================================================================
# 2D Image Processing routines based on Magick++ functions
# 1-to-1 R implementations of Magick::Image methods in stdFilters.cpp
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
# IMAGE PROCESSING ROUTINES via ImageMagick
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FIXME deprecate
adaptThresh <- thresh
#adaptThresh <- function(x, width = 20, height = 20, offset = 1000, preprocess = FALSE, modify = FALSE) {
#    .notImageError(x)
#    param = as.double(c(width, height, offset))
#    filter = as.integer(1)
#    if (!modify) {
#        if (preprocess)
#            x = gaussFilter(normalize(x), 4, 2)
#        else
#            x = copyImage(x)
#        return(.CallEBImage("stdFilter2D", x, filter, param))
#    }
#    else { # original data modified
#        if (preprocess) {
#            normalize(x, modify = TRUE)
#            gaussFilter(x, 4, 2, modify = TRUE)
#        }
#        invisible(.CallEBImage("stdFilter2D", x, filter, param))
#    }
#}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.blur <- function(x, radius = 1, sigma = 0.5, modify = TRUE) {
    .notImageError(x)
    param = as.double(c(radius, sigma))
    filter = as.integer(2)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
blur <- function(x, radius = 1, sigma = 0.5) {
    .blur(x, radius, sigma, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.contrast <- function(x, sharpen, modify = TRUE) {
    .notImageError(x)
    if (missing(sharpen))
        stop("argument 'sharpen' is essential")
    param = as.double(sharpen)
    filter = as.integer(3)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
contrast <- function(x, sharpen) {
    .contrast(x, sharpen, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.despeckle <- function(x, modify = TRUE) {
    .notImageError(x)
    filter = as.integer(4)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, NULL))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, NULL))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
despeckle <- function(x) {
    .despeckle(x, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.edge <- function(x, radius = 1, modify = TRUE) {
    .notImageError(x)
    param = as.double(radius)
    filter = as.integer(5)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
edge <- function(x, radius = 1) {
    .edge(x, radius)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.enhance <- function(x, modify = TRUE) {
    .notImageError(x)
    filter = as.integer(6)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, NULL))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, NULL))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
enhance <- function(x) {
    .enhance(x, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.equalize <- function(x, modify = TRUE) {
    .notImageError(x)
    filter = as.integer(7)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, NULL))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, NULL))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
equalize <- function(x) {
    .equalize(x, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ffill <- function(x, col, row, color, modify = FALSE) {
#    .notImageError(x)
#    if (missing(col) || missing(height))
#        stop("arguments 'col', 'row' and 'color' are essential")
#    if (x@rgb) {
#        red = getRed(as.integer(color))
#        green = getGreen(as.integer(color))
#        blue = getBlue(as.integer(color))
#        param = as.double(c(col, row, red, green, blue))
#    }
#    else
#        param = as.double(c(col, color))
#    filter = as.integer(8)
#    if (!modify) {
#        x = copyImage(x)
#        return(.CallEBImage("stdFilter2D", x, filter, param))
#    }
#    else # original data modified
#        invisible(.CallEBImage("stdFilter2D", x, filter, param))
#}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ffillEdge <- function(x, col, row, color, modify = FALSE) {
#    .notImageError(x)
#    if (missing(col) || missing(height))
#        stop("arguments 'col', 'row' and 'color' are essential")
#    if (x@rgb) {
#        red = getRed(as.integer(color))
#        green = getGreen(as.integer(color))
#        blue = getBlue(as.integer(color))
#        param = as.double(c(col, row, red, green, blue))
#    }
#    else
#        param = as.double(c(col, color))
#    filter = as.integer(9)
#    if (!modify) {
#        x = copyImage(x)
#        return(.CallEBImage("stdFilter2D", x, filter, param))
#    }
#    else # original data modified
#        invisible(.CallEBImage("stdFilter2D", x, filter, param))
#}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.colorGamma <- function(x, level, modify = TRUE) {
    .notImageError(x)
    if (missing(level))
        stop("argument 'level' is essential")
    param = as.double(level)
    filter = as.integer(10)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
colorGamma <- function(x, level) {
    .colorGamma(x, level, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.gaussFilter <- function(x, width = 1, sigma = 0.5, modify = TRUE) {
    .notImageError(x)
    param = as.double(c(width, sigma))
    filter = as.integer(11)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
gaussFilter <- function(x, width = 1, sigma = 0.5) {
    .gaussFilter(x, width, sigma, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.medianFilter <- function(x, radius = 2, modify = TRUE) {
    .notImageError(x)
    param = as.double(radius)
    filter = as.integer(12)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
medianFilter <- function(x, radius = 2) {
    .medianFilter(x, radius, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.mod <- function(x, brightness = 1, saturation = 1, hue = 1, modify = TRUE) {
    .notImageError(x)
    param = as.double(c(brightness, saturation, hue))
    filter = as.integer(13)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mod <- function(x, brightness = 1, saturation = 1, hue = 1) {
    .mod(x, brightness, saturation, hue, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.reduceNoise <- function(x, order = NULL, modify = TRUE) {
    .notImageError(x)
    if (is.null(order))
        order = -1
    param = as.double(order)
    filter = as.integer(14)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
reduceNoise <- function(x, order = NULL) {
    .reduceNoise(x, order, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rotate <- function(x, degrees = 90) {
    .notImageError(x)
    param = as.double(degrees)
    filter = as.integer(15)
    return(.CallEBImage("stdFilter2DRedim", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sampleImage <- function(x, dx, dy) {
    .notImageError(x)
    if (missing(dx) || missing(dy))
        stop("arguments 'dx' and 'dy' are essential")
    param = as.double(c(dx, dy))
    filter = as.integer(16)
    return(.CallEBImage("stdFilter2DRedim", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
scaleImage <- function(x, dx, dy) {
    .notImageError(x)
    if (missing(dx) || missing(dy))
        stop("arguments 'dx' and 'dy' are essential")
    param = as.double(c(dx, dy))
    filter = as.integer(17)
    return(.CallEBImage("stdFilter2DRedim", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.segment <- function(x, cluster = 1, smooth = 1.5, modify = TRUE) {
    .notImageError(x)
    param = as.double(c(cluster, smooth))
    filter = as.integer(18)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
segment <- function(x, cluster = 1, smooth = 1.5) {
    .segment(x, cluster, smooth, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.shade <- function(x, azimuth = 30, elevation = 30, shading = FALSE, modify = TRUE) {
    .notImageError(x)
    param = as.double(c(azimuth, elevation))
    filter = as.integer(19)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shade <- function(x, azimuth = 30, elevation = 30, shading = FALSE) {
    .shade(x, azimuth, elevation, shading, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.sharpen <- function(x, radius = 1, sigma = 0.5, modify = TRUE) {
    .notImageError(x)
    param = as.double(c(radius, sigma))
    filter = as.integer(20)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sharpen <- function(x, radius = 1, sigma = 0.5) {
    .sharpen(x, radius, sigma, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.solarize <- function(x, factor = 50, modify = TRUE) {
    .notImageError(x)
    param = as.double(factor)
    filter = as.integer(21)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
solarize <- function(x, factor = 50) {
    .solarize(x, factor, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.spread <- function(x, amount = 3, modify = TRUE) {
    .notImageError(x)
    param = as.double(amount)
    filter = as.integer(22)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
spread <- function(x, amount = 3) {
    .spread(x, amount, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.unsharpMask <- function(x, radius = 2, sigma = 0.5, amount = 5, threshold = 2, modify = TRUE) {
    .notImageError(x)
    param = as.double(c(radius, sigma, amount, threshold))
    filter = as.integer(23)
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unsharpMask <- function(x, radius = 2, sigma = 0.5, amount = 5, threshold = 2) {
    .unsharpMask(x, radius, sigma, amount, threshold, modify = FALSE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
.noise <- function(x, type = "gaussian", modify = TRUE) {
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
    if (!modify) {
        x = copyImage(x)
        return(.CallEBImage("stdFilter2D", x, filter, param))
    }
    else # original data modified
        invisible(.CallEBImage("stdFilter2D", x, filter, param))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# possible types: uniform, gaussian, multi(plicativeGaussian), impulse, laplace(ian), poisson
noise <- function(x, type = "gaussian") {
    .noise(x, type, modify = FALSE)
}
