# -------------------------------------------------------------------------
# Class Image, definition and methods
 
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

setClass("Image",
    representation(
        rgb        = "logical"
    ),
    prototype(
        rgb      = FALSE
    ),
    contains = "array"
)

# ============================================================================
# generics for internal use only
setGeneric(".normalize",    function(object, ...) standardGeneric(".normalize"))
setGeneric(".as.integer",   function(x, ...)      standardGeneric(".as.integer"))
setGeneric(".as.double",    function(x, ...)      standardGeneric(".as.double"))
setGeneric("correctType",   function(object)      standardGeneric("correctType"))
setGeneric("isCorrectType", function(object)      standardGeneric("isCorrectType"))

# ============================================================================
# CONSTRUCTORS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Image <- function(data = array(0, c(1, 1, 1)), dim = NULL, rgb = FALSE) {
    if (is.null(dim)) {
        if (!is.array(data))
            stop("Convert data to array of 2 or 3 dimensions or specify argument dim")
        dim = dim(data)
    }
    if (length(dim) == 2)
        dim <- c(dim, 1)
    if (length(dim) != 3)
        stop("Argument dim must be of length 2 (single images) or 3 (stacks)")
    new("Image", rgb = rgb, .Data =
      array(if (rgb) as.integer(data) else as.double(data), dim))
}
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is.validImage <- function(x) {
    if (!is(x, "Image")) {
        warning("Argument is not of class Image\n")
        return(FALSE)
    }
    if (length(dim(x)) != 3) {
        warning("Object class is Image, but data have wrong dimensionality: must be 3")
        return(FALSE)
    }
    return(TRUE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("assert", signature(object = "Image"),
    function(object, object2, ...) {
        if (missing(object2))
            return(is.validImage(object))
        if (!is.validImage(object) || !is.validImage(object2))
            return(FALSE)
        res <- TRUE
        eq <- (dim(object) == dim(object2))
        if (length(which(eq)) != 3) {
            warning("Images have different sizes in one or several dimensions")
            res <- FALSE
        }
        if (object@rgb != object2@rgb) {
            warning("Images have different color modes")
            res <- FALSE
        }
        return(res)
    }
)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.copyHeader <- function(x, newClass = "Image", rgb = FALSE) {
    if (!assert(x))
        stop("Wrong class of argument x, Image expected")
    if (rgb)
        res = new(newClass, .Data = integer(0), rgb = TRUE)
    else
        res = new(newClass, .Data = double(0), rgb = FALSE)
    # copy all fields here except data and rgb
    return(res)
}

# ============================================================================
# METHODS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("copy", signature(x = "Image"),
    function(x) {
        res = .copyHeader(x, class(x), rgb = x@rgb)
        if (x@rgb)
            res@.Data = array(as.integer(x@.Data), dim(x))
        else
            res@.Data = array(as.double(x@.Data), dim(x))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("display", signature(object = "Image"),
    function(object) {
        if (!isCorrectType(object))
            invisible(.CallEBImage("displayImages", correctType(object), FALSE))
        else
            invisible(.CallEBImage("displayImages", object, FALSE))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("channels", signature(object = "Image"),
    function(object) {
        res = list();
        if (!isCorrectType(object))
            object = correctType(object)
        res$red = getRed(object)
        res$green = getGreen(object)
        res$blue = getBlue(object)
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toGray", signature(object = "Image"),
    function(object) {
        if (!object@rgb)
            return(copy(object))
        if (!isCorrectType(object))
            tmp = .CallEBImage("any2gray", correctType(object))
        else
            tmp = .CallEBImage("any2gray", object)
        res = .copyHeader(object, class(object), FALSE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toRGB", signature(object = "Image"),
    function(object) {
        if (object@rgb)
            return(copy(object))
        if (!isCorrectType(object))
            tmp = .CallEBImage("any2rgb", correctType(object))
        else
            tmp = .CallEBImage("any2rgb", object)
        res = .copyHeader(object, class(object), TRUE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toX11char", signature(object = "Image"),
    function(object) {
        return(.CallEBImage("any2X11char", object@.Data))
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("add2RGB", signature(x = "Image", y = "ANY"),
    function(x, y) {
        if (length(y) != length(x))
            stop("length mismatch")
        if (!isCorrectType(x))
            tmp = .CallEBImage("add2rgb", correctType(x), y)
        else
            tmp = .CallEBImage("any2rgb", x, y)
        res = .copyHeader(x, class(x), TRUE)
        res@.Data = array(tmp, dim(x))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("sub2RGB", signature(x = "Image", y = "ANY"),
    function(x, y) {
        if (length(y) != length(x))
            stop("length mismatch")
        if (!isCorrectType(x))
            tmp = .CallEBImage("sub2rgb", correctType(x), y)
        else
            tmp = .CallEBImage("sub2rgb", x, y)
        res = .copyHeader(x, class(x), TRUE)
        res@.Data = array(tmp, dim(x))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("scale2RGB", signature(x = "Image", mult = "numeric"),
    function(x, mult) {
        if (!isCorrectType(x))
            tmp = .CallEBImage("scale2rgb", correctType(x), mult)
        else
            tmp = .CallEBImage("scale2rgb", x, mult)
        res = .copyHeader(x, class(x), TRUE)
        res@.Data = array(tmp, dim(x))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toRed", signature(object = "Image"),
    function(object) {
        if (!isCorrectType(object))
            tmp = .CallEBImage("asred", correctType(object))
        else
            tmp = .CallEBImage("asred", object)
        res = .copyHeader(object, class(object), TRUE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toGreen", signature(object = "Image"),
    function(object) {
        if (!isCorrectType(object))
            tmp = .CallEBImage("asgreen", correctType(object))
        else
            tmp = .CallEBImage("asgreen", object)
        res = .copyHeader(object, class(object), TRUE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toBlue", signature(object = "Image"),
    function(object) {
        if (!isCorrectType(object))
            tmp = .CallEBImage("asblue", correctType(object))
        else
            tmp = .CallEBImage("asblue", object)
        res = .copyHeader(object, class(object), TRUE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getRed", signature(object = "Image"),
    function(object) {
        if (!object@rgb)
            return(copy(object))
        if (!isCorrectType(object))
            tmp = .CallEBImage("getred", correctType(object))
        else
            tmp = .CallEBImage("getred", object)
        res = .copyHeader(object, class(object), FALSE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getGreen", signature(object = "Image"),
    function(object) {
        if (!object@rgb)
            return(copy(object))
        if (!isCorrectType(object))
            tmp = .CallEBImage("getgreen", correctType(object))
        else
            tmp = .CallEBImage("getgreen", object)
        res = .copyHeader(object, class(object), FALSE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getBlue", signature(object = "Image"),
    function(object) {
        if (!object@rgb)
            return(copy(object))
        if (!isCorrectType(object))
            tmp = .CallEBImage("getblue", correctType(object))
        else
            tmp = .CallEBImage("getblue", object)
        res = .copyHeader(object, class(object), FALSE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod(".normalize", signature(object = "Image"),
    function(object, from = 0, to = 1.0, independent = FALSE, modify = TRUE) {
        if (object@rgb)
            stop("Function supports grayscale images only")
        if (!modify) {
            res = copy(object)
            return(.CallEBImage("normalizeImages", res, as.double(c(from, to)), independent))
        }
        else
            invisible(.CallEBImage("normalizeImages", object, as.double(c(from, to)), independent))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("normalize", signature(object = "Image"),
    function(object, from = 0, to = 1.0, independent = FALSE) {
        .normalize(object, from, to, independent, modify = FALSE)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image", i = "missing", j = "missing"),
    function(x, i, j, k, ..., drop) {
        if (missing(k))
            return(x)
        tmp = x@.Data[ , , k, drop = FALSE]
        if(is.array(tmp)) {
            res = .copyHeader(x, "Image", x@rgb)
            res@.Data = tmp
            return(res)
        }
        else
            return(tmp)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image", i = "numeric", j = "missing"),
    function(x, i, j, k, ..., drop) {
        if (missing(k)) {
            warning("using index [int], cannot distinguish from [int,,ANY], use [int,1:dim(x)[2],ANY] otherwise")
            tmp = x@.Data[i, drop = FALSE]
        }
        else {
            tmp = x@.Data[i, , k, drop = FALSE]
        }
        if(is.array(tmp)) {
            res = .copyHeader(x, "Image", x@rgb)
            res@.Data = tmp
            return(res)
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image", i = "missing", j = "numeric"),
    function(x, i, j, k, ..., drop) {
        if (missing(k))
            k = 1:(dim(x@.Data)[3])
        tmp = x@.Data[ , j, k, drop = FALSE]
        if(is.array(tmp)) {
            res = .copyHeader(x, "Image", x@rgb)
            res@.Data = tmp
            return(res)
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image", i = "numeric", j = "numeric"),
    function(x, i, j, k, ..., drop) {
        if (missing(k))
            k = 1:(dim(x@.Data)[3])
        tmp = x@.Data[i, j, k, drop = FALSE]
        if(is.array(tmp)) {
            res = .copyHeader(x, "Image", x@rgb)
            res@.Data = tmp
            return(res)
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image", i = "array", j = "missing"),
    function(x, i, j, ..., drop) {
        #tmp = callGeneric(x@.Data, i, drop = FALSE)
        tmp = x@.Data[i, drop = FALSE]
        if(is.array(tmp)) {
            res = .copyHeader(x, class(x), x@rgb)
            res@.Data = tmp
            return(res)
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image", i = "logical", j = "missing"),
    function(x, i, j, ..., drop) {
        #tmp = callGeneric(x@.Data, i, drop = FALSE)
        tmp = x@.Data[i, drop = FALSE]
        if(is.array(tmp)) {
            res = .copyHeader(x, class(x), x@rgb)
            res@.Data = tmp
            return(res)
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", signature(object = "Image"),
    function(object) {
        print(object)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("print", signature(x = "Image"),
    function(x, data = FALSE, ...) {
        if (data) {
            print(x@.Data)
        }
        else {
            d = dim(x)
            if (x@rgb)
                cat(paste("Image (RGB, 8bit/col): ", d[3], " image(s) of ", d[1], "x", d[2], "\n", sep =""))
            else
                cat(paste("Image (grayscale, double): ", d[3], " image(s) of ", d[1], "x", d[2], "\n", sep =""))
            partial = rep(FALSE, 3)
            dmax = c(10, 5, 2)
            for(j in 1:3)
                if (d[j] > dmax[j]) {
                    d[j] = dmax[j]
                    partial[j] = TRUE
                }
            if(any(partial))
                cat("Showing ")
            if(any(partial[1:2]))
                cat(sprintf("rows 1:%d and columns 1:%d of ", d[1], d[2]))
            if(partial[3])
                cat(sprintf("images 1:%d\n", d[3]))
            if(any(partial))
                cat("\n")
            print(x@.Data[1:d[1], 1:d[2], 1:d[3]], digits=3)
            # if (!x@rgb)
            #     print(summary(as.numeric(x@.Data)))
        }
        invisible(NULL)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod(".as.integer", signature(x = "Image"),
    function(x, ...) {
        if (!x@rgb)
            stop("Function supports RGB images only")
        if (!is.integer(x))
            x@.Data = array(as.integer(x@.Data), dim(x@.Data))
        return(x)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod(".as.double", signature(x = "Image"),
    function(x, ...) {
        if (x@rgb)
            stop("Function supports grayscale images only")
        if (!is.double(x))
            x@.Data = array(as.double(x@.Data), dim(x@.Data))
        return(x)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("as.array", signature(x = "Image"),
    function(x) {
        return(x@.Data)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("isCorrectType", signature(object = "Image"),
    function(object) {
        if (object@rgb && !is.integer(object)) {
            warning("RGB image with data of non-integer type")
            return(FALSE)
        }
        if (!object@rgb && !is.double(object)) {
            warning("grayscale image with data of non-double type")
            return(FALSE)
        }
        return(TRUE)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("correctType", signature(object = "Image"),
    function(object) {
        if (object@rgb && !is.integer(object))
            object = .as.integer(object)
        else
            if (!object@rgb && !is.double(object))
                object = .as.double(object)
        return(object)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("summary", signature(object = "Image"),
    function(object, ...) {
        if (object@rgb)
            stop("Function supports grayscale images only")
        summary(as.numeric(object@.Data))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("plot.image", signature(x = "Image"),
    function(x, xlab = "", ylab = "", axes = FALSE, ...) {
        if (x@rgb)
            stop("Function defined for grayscale images only. Use display() instead")
        .dim <- dim(x)
        X <- 1:.dim[[1]]
        Y <- 1:.dim[[2]]
        asp <- .dim[[2]]/.dim[[1]]
        graphics:::image(x = X, y = Y, z = matrix(x[,,1], .dim[1:2]), asp=asp, col = gray((0:255)/255), axes=axes, xlab=xlab, ylab=ylab,  ...)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("write.image", signature(object = "Image", files = "character"),
    function(object, files) {
        files = as.character(files)
        if (!isCorrectType(object))
            object = correctType(object)
        invisible(.CallEBImage("writeImages", object, files))
    }
)
# ============================================================================
# ASSOCIATED ROUTINES
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read.image <- function(files, rgb = FALSE) {
    files = as.character(files)
    if (length(files) < 1)
        stop("At least one file/URL must be supplied")
    rgb = as.logical(rgb)[[1]]
    return(.CallEBImage("readImages", files, rgb))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ping.image <- function(files, show.comments = FALSE) {
    files = as.character(files)
    if (length(files) < 1)
        stop("At least one file/URL must be supplied")
    invisible(.CallEBImage("pingImages", files, as.logical(show.comments)))
}

