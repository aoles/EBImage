# ============================================================================
# Image3D: class definition and method for class 'Image3D' to represent
#          3D images
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copyright: Oleg Sklyar, 2005
#            European Bioinformatics Institute; Bioconductor.org
# ============================================================================
setClass("Image3D",
    contains = "Image2D"
)
# ============================================================================
# CONSTRUCTORS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Image3D <- function(data = array(0, c(2, 2, 2)), dim = NULL, rgb = FALSE) {
    if (!is.array(data) && is.null(dim))
        stop("'data' argument must be array or 'dim' argument must be specified")
    if (!is.null(dim))
        if (length(dim) > 3)
            warning("only three first elements of 'dim' will be used to create image matrix")
    res = new("Image3D", rgb = rgb)
    if (is.array(data) && is.null(dim))
        res@.Data = data
    else
        res@.Data = array(as.integer(data), dim[1:3])
    return(res)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Image3D.CopyHeader <- function(x, data = array(0, c(2, 2, 2)), dim = NULL) {
    if (!is(x, "Image3D"))
        stop("x must be of type 'Image3D'")
    if (!is.array(data) && is.null(dim))
        stop("'data' argument must be array or 'dim' argument must be specified")
    if (!is.null(dim))
        if (length(dim) > 3)
            warning("only three first elements of 'dim' will be used to create image matrix")
    # copy all fields here except data
    res = new("Image3D", rgb = x@rgb)
    # create data here
    if (is.array(data) && is.null(dim))
        res@.Data = data
    else
        res@.Data = array(as.integer(data), dim[1:3])
    return(res)
}
# ============================================================================
# METHODS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("normalize", signature(object = "Image3D"),
    function(object, from = 0, to = 65535) {
        minmax = min.max(object)
        if (minmax[[2]] - minmax[[1]] == 0)
            return(object)
        return(Image3D.CopyHeader(object, (object@.Data - minmax[[1]]) / (minmax[[2]] - minmax[[1]]) * (to - from) + from, dim(object)))
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("to.gray", signature(object = "Image3D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("toGray", as.integer(object))
        }
        else
            res = .CallImagine("toGray", object)
        res = Image3D.CopyHeader(object, res, dim(object))
        res@rgb = FALSE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("to.rgb", signature(object = "Image3D"),
    function(object) {
        if (object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("toRGB", as.integer(object))
        }
        else
            res = .CallImagine("toRGB", object)
        res = Image3D.CopyHeader(object, res, dim(object))
        res@rgb = TRUE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("to.red", signature(object = "Image3D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images are supported")
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("asRed", as.integer(object))
        }
        else
            res = .CallImagine("asRed", object)
        res = Image3D.CopyHeader(object, res, dim(object))
        res@rgb = TRUE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("to.green", signature(object = "Image3D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images are supported")
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("asGreen", as.integer(object))
        }
        else
            res = .CallImagine("asGreen", object)
        res = Image3D.CopyHeader(object, res, dim(object))
        res@rgb = TRUE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("to.blue", signature(object = "Image3D"),
    function(object) {
        if (object@rgb)
            stop("only grayscale images are supported")
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("asBlue", as.integer(object))
        }
        else
            res = .CallImagine("asBlue", object)
        res = Image3D.CopyHeader(object, res, dim(object))
        res@rgb = TRUE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("get.red", signature(object = "Image3D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("getRed", as.integer(object))
        }
        else
            res = .CallImagine("getRed", object)
        res = Image3D.CopyHeader(object, res, dim(object))
        res@rgb = FALSE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("get.green", signature(object = "Image3D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("getGreen", as.integer(object))
        }
        else
            res = .CallImagine("getGreen", object)
        res = Image3D.CopyHeader(object, res, dim(object))
        res@rgb = FALSE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("get.blue", signature(object = "Image3D"),
    function(object) {
        if (!object@rgb)
            return(object)
        if (!is.integer(object)) {
            warning("image data of type double... use as.integer on your image to correct")
            res = .CallImagine("getBlue", as.integer(object))
        }
        else
            res = .CallImagine("getBlue", object)
        res = Image3D.CopyHeader(object, res, dim(object))
        res@rgb = FALSE
        return(res)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image3D", i = "missing", j = "missing"),
    function(x, i, j, k, ..., drop) {
        #print("DEBUG: 3D missing missing")
        if (missing(k))
            return(x)
        tmp = x@.Data[ , , k]
        if(is.array(tmp)) {
            if (length(dim(tmp)) == 2)
                return(Image2D.CopyHeader(x, tmp))
            else
                return(Image3D.CopyHeader(x, tmp))
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image3D", i = "numeric", j = "missing"),
    function(x, i, j, k, ..., drop) {
        #print("DEBUG: 3D num missing")
        if (missing(k)) {
            warning("subscripts [int, , ANY] and [int] cannot be distinguished! [int] is used. Use [int,1:dim(x)[[2]],ANY] instead of [int, ,ANY]")
            tmp = callGeneric(x@.Data, i)
        }
        else {
            #j = 1:(dim(x@.Data)[[2]])
            #tmp = callGeneric(x@.Data, i, j, k)
            tmp = x@.Data[i, , k]
        }
        if(is.array(tmp)) {
            if (length(dim(tmp)) == 2)
                return(Image2D.CopyHeader(x, tmp))
            else
                return(Image3D.CopyHeader(x, tmp))
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image3D", i = "missing", j = "numeric"),
    function(x, i, j, k, ..., drop) {
        #print("DEBUG: 3D missing num")
        if (missing(k))
            k = 1:(dim(x@.Data)[[3]])
        #i = 1:(dim(x@.Data)[[1]])
        #tmp = callGeneric(x@.Data, i, j, k)
        tmp = x@.Data[ , j, k]
        if(is.array(tmp)) {
            if (length(dim(tmp)) == 2)
                return(Image2D.CopyHeader(x, tmp))
            else
                return(Image3D.CopyHeader(x, tmp))
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image3D", i = "numeric", j = "numeric"),
    function(x, i, j, k, ..., drop) {
        #print("DEBUG: 3D num num")
        if (missing(k))
            k = 1:(dim(x@.Data)[[3]])
        #tmp = callGeneric(x@.Data, i, j, k)
        tmp = x@.Data[i, j, k]
        if(is.array(tmp)) {
            if (length(dim(tmp)) == 2)
                return(Image2D.CopyHeader(x, tmp))
            else
                return(Image3D.CopyHeader(x, tmp))
        }
        else
            return(tmp)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", signature(object = "Image3D"),
    function(object) {
        .dim = dim(object)
        cat(paste("Image3D: ", .dim[[3]], " images of ", .dim[[1]], "x", .dim[[2]], "\n", sep =""))
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
#            if (.dim[[3]] > 2) {
#                .dim[[3]] = 2
#                partial = TRUE
#            }
#            if (partial)
#                cat("\tImage is too large, printing only max 10x10 data matrix.\n\tSet PRINT_FULL_DATA=TRUE to enable 'show' to print all data\n")
#        }
#        print(object@.Data[1:.dim[[1]], 1:.dim[[2]], 1:.dim[[3]]])
        if (!object@rgb)
            print(summary(as.numeric(object@.Data)))
        if (PRINT_DATA)
            print(object@.Data)
        invisible(NULL)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("as.integer", signature(x = "Image3D"),
    function(x, ...) {
        if (is.integer(x))
            return(x)
        else
            return(Image3D.CopyHeader(x, as.integer(x@.Data), dim(x)))
    }
)
