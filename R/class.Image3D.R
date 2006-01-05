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
        dim = dim(data)
    res = new("Image2D", rgb = rgb)
    if (rgb)
        res@.Data = array(as.integer(data), dim[1:3])
    else
        res@.Data = array(as.double(data), dim[1:3])
    return(res)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# DEPRECATED: copyImageHeader instead fully defined in Image2D
#Image3D.CopyHeader <- function(x, data = array(0, c(2, 2, 2)), dim = NULL) {
#    if (!is(x, "Image3D"))
#        stop("x must be of type 'Image3D'")
#    if (!is.array(data) && is.null(dim))
#        stop("'data' argument must be array or 'dim' argument must be specified")
#    if (!is.null(dim))
#        if (length(dim) > 3)
#            warning("only three first elements of 'dim' will be used to create image matrix")
#    if (is.array(data) && is.null(dim))
#        dim = dim(data)
#    # copy all fields here except data
#    res = new("Image3D", rgb = x@rgb)
#    # create data here
#    if (x@rgb)
#        res@.Data = array(as.integer(data), dim[1:3])
#    else
#        res@.Data = array(as.double(data), dim[1:3])
#    return(res)
#}
# ============================================================================
# METHODS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Image2D implementation is sufficient
#setMethod("normalize", signature(object = "Image3D"),
#    function(object, from = 0, to = 1.0, this = FALSE, independent = FALSE) {
#        if (object@rgb)
#            stop("only grayscale images supported so far")
#        if (this == FALSE) {
#            if (!independent) {
#                minmax = minMax(object)
#                if (minmax[[2]] - minmax[[1]] == 0)
#                    return(object)
#                res = copyImageHeader(object, "Image3D", FALSE)
#                res@.Data = (object@.Data - minmax[[1]]) / (minmax[[2]] - minmax[[1]]) * (to - from) + from
#                return(res)
#            }
#            else {
#                res = object
#                nimages = dim(object)[[3]]
#                for (i in 1:nimages)
#                    res[,,i] = normalize(res[,,i]) # should invoke normalize for Image2D
#                return(res)
#            }
#        }
#        else {
#            invisible(.CallEBImage("normalizeImages", object, as.double(c(from, to)), independent))
#        }
#    }
#)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image3D", i = "missing", j = "missing"),
    function(x, i, j, k, ..., drop) {
        #print("DEBUG: 3D missing missing")
        if (missing(k))
            return(x)
        tmp = x@.Data[ , , k]
        if(is.array(tmp)) {
            if (length(dim(tmp)) == 2)
                res = copyImageHeader(x, rgb = x@rgb)
            else
                res = copyImageHeader(x, "Image3D", x@rgb)
            res@.Data = tmp
            return(res)
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
                res = copyImageHeader(x, rgb = x@rgb)
            else
                res = copyImageHeader(x, "Image3D", x@rgb)
            res@.Data = tmp
            return(res)
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
                res = copyImageHeader(x, rgb = x@rgb)
            else
                res = copyImageHeader(x, "Image3D", x@rgb)
            res@.Data = tmp
            return(res)
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
                res = copyImageHeader(x, rgb = x@rgb)
            else
                res = copyImageHeader(x, "Image3D", x@rgb)
            res@.Data = tmp
            return(res)
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
            cat(paste("\tType: grayscale, doubles in the range [0..1]\n"))
        if (!PRINT_ALL_DATA) {
            partial = FALSE
            if (.dim[[1]] > 10) {
                .dim[[1]] = 10
                partial = TRUE
            }
            if (.dim[[2]] > 10) {
                .dim[[2]] = 10
                partial = TRUE
            }
            if (.dim[[3]] > 2) {
                .dim[[3]] = 2
                partial = TRUE
            }
            if (partial)
                cat("\tImage is too large, printing only max 10x10 data matrix.\n\tSet PRINT_ALL_DATA=TRUE to enable 'show' to print all data\n")
        }
        print(object@.Data[1:.dim[[1]], 1:.dim[[2]], 1:.dim[[3]]])
#        if (!object@rgb)
#            print(summary(as.numeric(object@.Data)))
        invisible(NULL)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Current Image2D implementation should be sufficient
#setMethod("as.integer", signature(x = "Image3D"),
#    function(x, ...) {
#        if (is.integer(x))
#            return(x)
#        else
#            return(Image3D.CopyHeader(x, as.integer(x@.Data), dim(x)))
#    }
#)
