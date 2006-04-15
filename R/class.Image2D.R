# ============================================================================
# Image2D: class definition and method for class 'Image2D' to represent
#          2D images
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
setClass("Image2D",
    representation(
        rgb        = "logical"
    ),
    prototype(
        rgb      = FALSE
    ),
    contains = "array"
)
# ============================================================================
PRINT_ALL_DATA = FALSE
# ============================================================================
# NON-STANDARD GENERICS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
setGeneric("minMax",      function(object)      standardGeneric("minMax"))
setGeneric("as.integer",  function(x, ...)      standardGeneric("as.integer"))
setGeneric("as.double",   function(x, ...)      standardGeneric("as.double"))
setGeneric("as.array",    function(x)           standardGeneric("as.array"))
setGeneric("summary",     function(object, ...) standardGeneric("summary"))
setGeneric("correctType", function(object)      standardGeneric("correctType"))
setGeneric("isCorrectType", function(object)    standardGeneric("isCorrectType"))
# ============================================================================
# CONSTRUCTORS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Image2D <- function(data = array(0, c(2, 2)), dim, rgb = FALSE) {
  if (!missing(dim)) {
    if (length(dim) != 2)
      stop("'dim' must be vector of length 2.")
  } else {
    if (!is.array(data))
      stop("If 'dim' is not specified, 'data' must be an array.")
    dim = dim(data)
  }
  new("Image2D", rgb = rgb, .Data =
      array(if (rgb) as.integer(data) else as.double(data), dim))
}
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
copyImageHeader <- function(x, newClass = "Image2D", rgb = FALSE) {
    .notImageError(x)
    if (rgb)
        res = new(newClass, .Data = integer(0), rgb = TRUE)
    else
        res = new(newClass, .Data = double(0), rgb = FALSE)
    # copy all fields here except data and rgb
    return(res)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
copyImage <- function(x) {
    .notImageError(x)
    res = copyImageHeader(x, class(x), rgb = x@rgb)
    if (x@rgb)
        res@.Data = array(as.integer(x@.Data), dim(x))
    else
        res@.Data = array(as.double(x@.Data), dim(x))
    return(res)
}
# ============================================================================
# METHODS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("display", signature(object = "Image2D"),
    function(object) {
        if (!isCorrectType(object))
            invisible(.CallEBImage("displayImages", correctType(object), FALSE))
        else
            invisible(.CallEBImage("displayImages", object, FALSE))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("channels", signature(object = "Image2D"),
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
setMethod("toGray", signature(object = "Image2D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!isCorrectType(object))
            tmp = .CallEBImage("toGray", correctType(object))
        else
            tmp = .CallEBImage("toGray", object)
        res = copyImageHeader(object, class(object), FALSE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toRGB", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            return(object)
        if (!isCorrectType(object))
            tmp = .CallEBImage("toRGB", correctType(object))
        else
            tmp = .CallEBImage("toRGB", object)
        res = copyImageHeader(object, class(object), TRUE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toRed", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images are supported")
        if (!isCorrectType(object))
            tmp = .CallEBImage("asRed", correctType(object))
        else
            tmp = .CallEBImage("asRed", object)
        res = copyImageHeader(object, class(object), TRUE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toGreen", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images are supported")
        if (!isCorrectType(object))
            tmp = .CallEBImage("asGreen", correctType(object))
        else
            tmp = .CallEBImage("asGreen", object)
        res = copyImageHeader(object, class(object), TRUE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toBlue", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images are supported")
        if (!isCorrectType(object))
            tmp = .CallEBImage("asBlue", correctType(object))
        else
            tmp = .CallEBImage("asBlue", object)
        res = copyImageHeader(object, class(object), TRUE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getRed", signature(object = "Image2D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!isCorrectType(object))
            tmp = .CallEBImage("getRed", correctType(object))
        else
            tmp = .CallEBImage("getRed", object)
        res = copyImageHeader(object, class(object), FALSE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getGreen", signature(object = "Image2D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!isCorrectType(object))
            tmp = .CallEBImage("getGreen", correctType(object))
        else
            tmp = .CallEBImage("getGreen", object)
        res = copyImageHeader(object, class(object), FALSE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getBlue", signature(object = "Image2D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!isCorrectType(object))
            tmp = .CallEBImage("getBlue", correctType(object))
        else
            tmp = .CallEBImage("getBlue", object)
        res = copyImageHeader(object, class(object), FALSE)
        res@.Data = array(tmp, dim(object))
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("normalize", signature(object = "Image2D"),
    function(object, from = 0, to = 1.0, modify = FALSE, independent = FALSE) {
        if (object@rgb)
            stop("only grayscale images supported so far")
        if (!modify) {
            res = copyImage(object)
            return(.CallEBImage("normalizeImages", res, as.double(c(from, to)), independent))
        }
        else
            invisible(.CallEBImage("normalizeImages", object, as.double(c(from, to)), independent))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image2D", i = "missing", j = "missing"),
    function(x, i, j, ..., drop) {
        return(x)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image2D", i = "numeric", j = "missing"),
    function(x, i, j, ..., drop) {
        warning("subscripts [int, ] and [int] cannot be distinguished! [int] is used. Use [int,1:dim(x)[2]] instead of [int,]")
        tmp = callGeneric(x@.Data, i)
        if(is.array(tmp)) {
            res = copyImageHeader(x, class(x), x@rgb)
            res@.Data = tmp
            return(res)
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image2D", i = "missing", j = "numeric"),
    function(x, i, j, ..., drop) {
        i = 1:(dim(x@.Data)[1])
        tmp = callGeneric(x@.Data, i, j)
        if(is.array(tmp)) {
            res = copyImageHeader(x, class(x), x@rgb)
            res@.Data = tmp
            return(res)
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image2D", i = "array", j = "missing"),
    function(x, i, j, ..., drop) {
        tmp = callGeneric(x@.Data, i)
        if(is.array(tmp)) {
            res = copyImageHeader(x, class(x), x@rgb)
            res@.Data = tmp
            return(res)
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image2D", i = "logical", j = "missing"),
    function(x, i, j, ..., drop) {
        tmp = callGeneric(x@.Data, i)
        if(is.array(tmp)) {
            res = copyImageHeader(x, class(x), x@rgb)
            res@.Data = tmp
            return(res)
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image2D", i = "numeric", j = "numeric"),
    function(x, i, j, ..., drop) {
        tmp = callGeneric(x@.Data, i, j)
        if(is.array(tmp)) {
            res = copyImageHeader(x, class(x), x@rgb)
            res@.Data = tmp
            return(res)
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", signature(object = "Image2D"),
    function(object) {
        d = dim(object)
        cat(paste("Image2D: ", d[1], "x", d[2], "\n", sep =""))
        if (object@rgb)
            cat("\tType: RGB, 8-bit per color\n")
        else
            cat(paste("\tType: grayscale, double precision\n"))
        if (!PRINT_ALL_DATA) {
            partial = rep(FALSE, 2)
            for(j in 1:2)
              if (d[j] > 10) {
                d[j] = 10
                partial[j] = TRUE
              }
            if (any(partial))
                cat(sprintf("\tShowing rows 1:%d and columns 1:%d\n", d[1], d[2]))
        }
        print(object@.Data[1:d[1], 1:d[2]])
#        if (!object@rgb)
#            print(summary(as.numeric(object@.Data)))
        invisible(NULL)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("minMax", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images supported so far")
        return(c(min(object@.Data), max(object@.Data)))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("as.integer", signature(x = "Image2D"),
    function(x, ...) {
        if (!x@rgb)
            stop("as.integer cannot be used on grayscale images")
        if (!is.integer(x))
            x@.Data = array(as.integer(x@.Data), dim(x@.Data))
        return(x)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("as.double", signature(x = "Image2D"),
    function(x, ...) {
        if (x@rgb)
            stop("as.double cannot be used on RGB images")
        if (!is.double(x))
            x@.Data = array(as.double(x@.Data), dim(x@.Data))
        return(x)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("as.array", signature(x = "Image2D"),
    function(x) {
        return(x@.Data)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("isCorrectType", signature(object = "Image2D"),
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
setMethod("correctType", signature(object = "Image2D"),
    function(object) {
        if (object@rgb && !is.integer(object))
            object = as.integer(object)
        else
            if (!object@rgb && !is.double(object))
                object = as.double(object)
        return(object)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("summary", signature(object = "Image2D"),
    function(object, ...) {
        if (object@rgb)
            stop("only grayscale images supported so far in method 'summary'")
        summary(as.numeric(object@.Data))
    }
)
# ============================================================================
# INTERNALS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.notImageError <- function(x) {
    if (!is(x, "Image2D"))
        stop("argument must be of class 'Image2D' or 'Image3D'")
    invisible(TRUE)
}

