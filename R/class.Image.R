# ============================================================================
# Image: class definition and method for class 'Image'
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005-2006
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
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
# NON-STANDARD GENERICS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setGeneric("assert",      function(object, ...) standardGeneric("assert"))
setGeneric("copy",        function(x)           standardGeneric("copy"))
setGeneric("display",     function(object, ...) standardGeneric("display"))
setGeneric("channels",    function(object)      standardGeneric("channels"))
setGeneric("toGray",      function(object)      standardGeneric("toGray"))
setGeneric("toRGB",       function(object)      standardGeneric("toRGB"))
setGeneric("toRed",       function(object)      standardGeneric("toRed"))
setGeneric("toGreen",     function(object)      standardGeneric("toGreen"))
setGeneric("toBlue",      function(object)      standardGeneric("toBlue"))
setGeneric("getRed",      function(object)      standardGeneric("getRed"))
setGeneric("getGreen",    function(object)      standardGeneric("getGreen"))
setGeneric("getBlue",     function(object)      standardGeneric("getBlue"))
setGeneric("normalize",   function(object, ...) standardGeneric("normalize"))
setGeneric("as.array",    function(x)           standardGeneric("as.array"))
setGeneric("summary",     function(object, ...) standardGeneric("summary"))

# FIXME deprecate
setGeneric("minMax",      function(object)      standardGeneric("minMax"))

# FOR INTERNAL USE BY THE DEVELOPERS ONLY (segmentation fault risk!)
setGeneric(".normalize",  function(object, ...) standardGeneric(".normalize"))
setGeneric(".as.integer", function(x, ...)      standardGeneric(".as.integer"))
setGeneric(".as.double",  function(x, ...)      standardGeneric(".as.double"))
setGeneric("correctType", function(object)      standardGeneric("correctType"))
setGeneric("isCorrectType", function(object)    standardGeneric("isCorrectType"))
# ============================================================================
# CONSTRUCTORS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Image <- function(data = array(0, c(1, 1, 1)), dim, rgb = FALSE) {
    if (missing(dim)) {
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
## FIXME deprecate both
Image2D <- Image
Image3D <- Image
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
## FIXME deprecate
copyImage <- function(x) {
    return(copy(x))
}
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
            return(object)
        if (!isCorrectType(object))
            tmp = .CallEBImage("toGray", correctType(object))
        else
            tmp = .CallEBImage("toGray", object)
        res = .copyHeader(object, class(object), FALSE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toRGB", signature(object = "Image"),
    function(object) {
        if (object@rgb)
            return(object)
        if (!isCorrectType(object))
            tmp = .CallEBImage("toRGB", correctType(object))
        else
            tmp = .CallEBImage("toRGB", object)
        res = .copyHeader(object, class(object), TRUE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toRed", signature(object = "Image"),
    function(object) {
        if (object@rgb)
            stop("Function supports grayscale images only")
        if (!isCorrectType(object))
            tmp = .CallEBImage("asRed", correctType(object))
        else
            tmp = .CallEBImage("asRed", object)
        res = .copyHeader(object, class(object), TRUE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toGreen", signature(object = "Image"),
    function(object) {
        if (object@rgb)
            stop("Function supports grayscale images only")
        if (!isCorrectType(object))
            tmp = .CallEBImage("asGreen", correctType(object))
        else
            tmp = .CallEBImage("asGreen", object)
        res = .copyHeader(object, class(object), TRUE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toBlue", signature(object = "Image"),
    function(object) {
        if (object@rgb)
            stop("Function supports grayscale images only")
        if (!isCorrectType(object))
            tmp = .CallEBImage("asBlue", correctType(object))
        else
            tmp = .CallEBImage("asBlue", object)
        res = .copyHeader(object, class(object), TRUE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getRed", signature(object = "Image"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!isCorrectType(object))
            tmp = .CallEBImage("getRed", correctType(object))
        else
            tmp = .CallEBImage("getRed", object)
        res = .copyHeader(object, class(object), FALSE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getGreen", signature(object = "Image"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!isCorrectType(object))
            tmp = .CallEBImage("getGreen", correctType(object))
        else
            tmp = .CallEBImage("getGreen", object)
        res = .copyHeader(object, class(object), FALSE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getBlue", signature(object = "Image"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!isCorrectType(object))
            tmp = .CallEBImage("getBlue", correctType(object))
        else
            tmp = .CallEBImage("getBlue", object)
        res = .copyHeader(object, class(object), FALSE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod(".normalize", signature(object = "Image"),
    function(object, from = 0, to = 1.0, modify = FALSE, independent = FALSE) {
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
        .normalize(object, from, to, modify = FALSE, independent)
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
            warning("Undistinguishable [int, , ANY] and [int], using [int]. Use [int,1:dim(x)[2],ANY] for [int, , ANY]")
            #tmp = callGeneric(x@.Data, i, drop = FALSE)
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
            res = .copyHeader(x, "Image3D", x@rgb)
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
# FIXME deprecate
setMethod("minMax", signature(object = "Image"),
    function(object) {
        if (object@rgb)
            stop("Function supports grayscale images only")
        return(range(object@.Data))
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
