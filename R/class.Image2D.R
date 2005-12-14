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
PRINT_DATA = FALSE
# ============================================================================
# NON-STANDARD GENERICS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setGeneric("display",   function(object, ...) standardGeneric("display"))
setGeneric("channels",  function(object)      standardGeneric("channels"))
setGeneric("toGray",    function(object)      standardGeneric("toGray"))
setGeneric("toRGB",     function(object)      standardGeneric("toRGB"))
setGeneric("toRed",     function(object)      standardGeneric("toRed"))
setGeneric("toGreen",   function(object)      standardGeneric("toGreen"))
setGeneric("toBlue",    function(object)      standardGeneric("toBlue"))
setGeneric("getRed",    function(object)      standardGeneric("getRed"))
setGeneric("getGreen",  function(object)      standardGeneric("getGreen"))
setGeneric("getBlue",   function(object)      standardGeneric("getBlue"))
setGeneric("normalize", function(object, ...) standardGeneric("normalize"))
setGeneric("to16bit",   function(object, ...) standardGeneric("to16bit"))
setGeneric("minMax",    function(object)      standardGeneric("minMax"))
setGeneric("as.integer",function(x, ...)      standardGeneric("as.integer"))
setGeneric("as.array",  function(x)           standardGeneric("as.array"))
setGeneric("summary",   function(object, ...) standardGeneric("summary"))
# ============================================================================
# CONSTRUCTORS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Image2D <- function(data = array(0, c(2, 2)), dim = NULL, rgb = FALSE) {
    if (!is.array(data) && is.null(dim))
        stop("'data' argument must be array or 'dim' argument must be specified")
    if (!is.null(dim))
        if (length(dim) > 2)
            warning("only two first elements of 'dim' will be used to create image matrix")
    res = new("Image2D", rgb = rgb)
    if (is.array(data) && is.null(dim))
        res@.Data = data
    else
        res@.Data = array(as.integer(data), dim[1:2])
    return(res)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Image2D.CopyHeader <- function(x, data = array(0, c(2, 2)), dim = NULL) {
    if (!is(x, "Image2D"))
        stop("x must be of type 'Image2D'")
    if (!is.array(data) && is.null(dim))
        stop("'data' argument must be array or 'dim' argument must be specified")
    if (!is.null(dim))
        if (length(dim) > 2)
            warning("only two first elements of 'dim' will be used to create image matrix")
    # copy all fields here except data
    res = new("Image2D", rgb = x@rgb)
    # create data here
    if (is.array(data) && is.null(dim))
        res@.Data = data
    else
        res@.Data = array(as.integer(data), dim[1:2])
    return(res)
}
# ============================================================================
# METHODS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("display", signature(object = "Image2D"),
    function(object) {
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            invisible(.CallEBImage("displayImages", as.integer(object), FALSE))
        }
        else
            invisible(.CallEBImage("displayImages", object, FALSE))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("channels", signature(object = "Image2D"),
    function(object) {
        res = list();
        res$red = get.red(object)
        res$green = get.green(object)
        res$blue = get.blue(object)
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toGray", signature(object = "Image2D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallEBImage("toGray", as.integer(object))
        }
        else
            res = .CallEBImage("toGray", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = FALSE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toRGB", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallEBImage("toRGB", as.integer(object))
        }
        else
            res = .CallEBImage("toRGB", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = TRUE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toRed", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images are supported")
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallEBImage("asRed", as.integer(object))
        }
        else
            res = .CallEBImage("asRed", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = TRUE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toGreen", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images are supported")
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallEBImage("asGreen", as.integer(object))
        }
        else
            res = .CallEBImage("asGreen", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = TRUE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("toBlue", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images are supported")
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallEBImage("asBlue", as.integer(object))
        }
        else
            res = .CallEBImage("asBlue", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = TRUE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getRed", signature(object = "Image2D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallEBImage("getRed", as.integer(object))
        }
        else
            res = .CallEBImage("getRed", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = FALSE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getGreen", signature(object = "Image2D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallEBImage("getGreen", as.integer(object))
        }
        else
            res = .CallEBImage("getGreen", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = FALSE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getBlue", signature(object = "Image2D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallEBImage("getBlue", as.integer(object))
        }
        else
            res = .CallEBImage("getBlue", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = FALSE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("normalize", signature(object = "Image2D"),
    function(object, from = 0, to = 65535) {
        minmax = minMax(object)
        if (minmax[[2]] - minmax[[1]] == 0)
            return(object)
        return(Image2D.CopyHeader(object, (object@.Data - minmax[[1]]) / (minmax[[2]] - minmax[[1]]) * (to - from) + from, dim(object)))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("to16bit", signature(object = "Image2D"),
    function(object, bits = 12, ...) {
        if (object@rgb)
            stop("to16bit can be used on grayscale objects only")
        if (missing(bits))
            bits = 12
        factor = 2 ^ (16 - bits)
        return(as.integer((object + 1) * factor - 1))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image2D", i = "missing", j = "missing"),
    function(x, i, j, ..., drop) {
        #print("DEBUG: 2D missing missing")
        return(x)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image2D", i = "numeric", j = "missing"),
    function(x, i, j, ..., drop) {
        #print("DEBUG: 2D num missing")
        warning("subscripts [int, ] and [int] cannot be distinguished! [int] is used. Use [int,1:dim(x)[[2]]] instead of [int,]")
        tmp = callGeneric(x@.Data, i)
        if(is.array(tmp))
            return(Image2D.CopyHeader(x, tmp))
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image2D", i = "missing", j = "numeric"),
    function(x, i, j, ..., drop) {
        #print("DEBUG: 2D missing num")
        i = 1:(dim(x@.Data)[[1]])
        tmp = callGeneric(x@.Data, i, j)
        if(is.array(tmp))
            return(Image2D.CopyHeader(x, tmp))
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image2D", i = "array", j = "missing"),
    function(x, i, j, ..., drop) {
        #print("DEBUG: 2D array missing")
        tmp = callGeneric(x@.Data, i)
        if(is.array(tmp))
            return(Image2D.CopyHeader(x, tmp))
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image2D", i = "logical", j = "missing"),
    function(x, i, j, ..., drop) {
        #print("DEBUG: 2D logical missing")
        tmp = callGeneric(x@.Data, i)
        if(is.array(tmp))
            return(Image2D.CopyHeader(x, tmp))
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image2D", i = "numeric", j = "numeric"),
    function(x, i, j, ..., drop) {
        #print("DEBUG: 2D numeric numeric")
        tmp = callGeneric(x@.Data, i, j)
        if(is.array(tmp))
            return(Image2D.CopyHeader(x, tmp))
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", signature(object = "Image2D"),
    function(object) {
        .dim = dim(object)
        cat(paste("Image2D: ", .dim[[1]], "x", .dim[[2]], "\n", sep =""))
        if (object@rgb)
            cat("\tType: RGB, 8-bit per color\n")
        else
            cat(paste("\tType: grayscale 16 bit with white level 65535\n"))
#        if (!PRINT_FULL_DATA) {
#            partial = FALSE
#            if (.dim[[1]] > 10) {
#                .dim[[1]] = 10
#                partial = TRUE
#            }
#            if (.dim[[2]] > 10) {
#                .dim[[2]] = 10
#                partial = TRUE
#            }
#            if (partial)
#                cat("\tImage is too large, printing only max 10x10 data matrix.\n\tSet PRINT_FULL_DATA=TRUE to enable 'show' to print all data\n")
#        }
#        print(object@.Data[1:.dim[[1]], 1:.dim[[2]]])
        if (!object@rgb)
            print(summary(as.numeric(object@.Data)))
        if (PRINT_DATA)
            print(object@.Data)
        invisible(NULL)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("minMax", signature(object = "Image2D"),
    function(object) {
        res = integer(2)
        res[[1]] = min(object@.Data)
        res[[2]] = max(object@.Data)
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("as.integer", signature(x = "Image2D"),
    function(x, ...) {
        if (is.integer(x))
            return(x)
        else
            return(Image2D.CopyHeader(x, as.integer(x@.Data), dim(x)))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("as.array", signature(x = "Image2D"),
    function(x) {
        return(x@.Data)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("summary", signature(object = "Image2D"),
    function(object, ...) {
        summary(as.numeric(object@.Data))
    }
)