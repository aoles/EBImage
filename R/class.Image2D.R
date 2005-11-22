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
setGeneric("to.gray",   function(object)      standardGeneric("to.gray"))
setGeneric("to.rgb",    function(object)      standardGeneric("to.rgb"))
setGeneric("to.red",    function(object)      standardGeneric("to.red"))
setGeneric("to.green",  function(object)      standardGeneric("to.green"))
setGeneric("to.blue",   function(object)      standardGeneric("to.blue"))
setGeneric("get.red",   function(object)      standardGeneric("get.red"))
setGeneric("get.green", function(object)      standardGeneric("get.green"))
setGeneric("get.blue",  function(object)      standardGeneric("get.blue"))
setGeneric("normalize", function(object, ...) standardGeneric("normalize"))
setGeneric("to16bit",   function(object, ...) standardGeneric("to16bit"))
setGeneric("min.max",   function(object)      standardGeneric("min.max"))
setGeneric("as.integer", function(x, ...)     standardGeneric("as.integer"))
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
            invisible(.CallImagine("displayImages", as.integer(object), FALSE))
        }
        else
            invisible(.CallImagine("displayImages", object, FALSE))
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
setMethod("to.gray", signature(object = "Image2D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("toGray", as.integer(object))
        }
        else
            res = .CallImagine("toGray", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = FALSE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("to.rgb", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("toRGB", as.integer(object))
        }
        else
            res = .CallImagine("toRGB", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = TRUE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("to.red", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images are supported")
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("asRed", as.integer(object))
        }
        else
            res = .CallImagine("asRed", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = TRUE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("to.green", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images are supported")
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("asGreen", as.integer(object))
        }
        else
            res = .CallImagine("asGreen", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = TRUE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("to.blue", signature(object = "Image2D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images are supported")
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("asBlue", as.integer(object))
        }
        else
            res = .CallImagine("asBlue", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = TRUE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("get.red", signature(object = "Image2D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("getRed", as.integer(object))
        }
        else
            res = .CallImagine("getRed", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = FALSE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("get.green", signature(object = "Image2D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("getGreen", as.integer(object))
        }
        else
            res = .CallImagine("getGreen", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = FALSE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("get.blue", signature(object = "Image2D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("getBlue", as.integer(object))
        }
        else
            res = .CallImagine("getBlue", object)
        res = Image2D.CopyHeader(object, res, dim(object))
        res@rgb = FALSE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("normalize", signature(object = "Image2D"),
    function(object, from = 0, to = 65535) {
        minmax = min.max(object)
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
setMethod("min.max", signature(object = "Image2D"),
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