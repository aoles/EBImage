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







any2rgb <- function(x) {
    return(.CallEBImage("any2rgb", x))
}

any2gray <- function(x) {
    return(.CallEBImage("any2gray", x))
}

any2X11char <- function(x) {
    return(.CallEBImage("any2X11char", x))
}

add2rgb <- function(x, y) {
    return(.CallEBImage("add2rgb", x, y))
}

sub2rgb <- function(x, y) {
    return(.CallEBImage("sub2rgb", x, y))
}

scale2rgb <- function(x, factor) {
    return(.CallEBImage("scale2rgb", x, factor))
}
