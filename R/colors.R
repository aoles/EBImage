colString <- function(r, g, b, gray) {
    if (!missing(gray)) {
        gray <- as.double(gray)
        return(.CallEBImage("toColorString", gray))
    }
    else {
        if(missing(r) || missing(g) || missing(b))
            stop("All arguments are essential")
        rgbx <- as.double(c(r, g, b))
        return(.CallEBImage("toColorString", rgbx))
    }
}

colRGB <- function(col) {
    if (missing(col))
        stop("Please provide color string in X11 format")
    col <- as.character(col)
    return(.CallEBImage("fromColorString", col))
}

colString1 <- function(x) {
    return(.CallEBImage("intToColorString", x))
}

colRGB1 <- function(x) {
    return(.CallEBImage("colorStringToInt", x))
}
