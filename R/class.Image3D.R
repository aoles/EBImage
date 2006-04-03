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
# ============================================================================
# METHODS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", signature(x = "Image3D", i = "missing", j = "missing"),
    function(x, i, j, k, ..., drop) {
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
        if (missing(k)) {
            warning("subscripts [int, , ANY] and [int] cannot be distinguished! [int] is used. Use [int,1:dim(x)[[2]],ANY] instead of [int, ,ANY]")
            tmp = callGeneric(x@.Data, i)
        }
        else {
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
        if (missing(k))
            k = 1:(dim(x@.Data)[[3]])
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
        if (missing(k))
            k = 1:(dim(x@.Data)[[3]])
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
                cat("\tPrinting only max 10x10 values of the first image, image is too large\n")
        }
        print(object@.Data[1:.dim[[1]], 1:.dim[[2]], 1:.dim[[3]]])
#        if (!object@rgb)
#            print(summary(as.numeric(object@.Data)))
        invisible(NULL)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
