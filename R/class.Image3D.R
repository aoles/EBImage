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
Image3D <- function(data = array(0, c(2, 2, 2)), dim, rgb = FALSE) {
  if (!missing(dim)) {
    if (length(dim) != 3)
      stop("'dim' must be vector of length 3.")
  } else {
    if (!is.array(data))
      stop("If 'dim' is not specified, 'data' must be an array.")
    dim = dim(data)
  }
  new("Image3D", rgb = rgb, .Data =
      array(if (rgb) as.integer(data) else as.double(data), dim))
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
            warning("subscripts [int, , ANY] and [int] cannot be distinguished! [int] is used. Use [int,1:dim(x)[2],ANY] instead of [int, ,ANY]")
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
            k = 1:(dim(x@.Data)[3])
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
            k = 1:(dim(x@.Data)[3])
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
        d = dim(object)
        cat(paste("Image3D: ", d[3], " images of size ", d[1], "x", d[2], "\n", sep =""))
        if (object@rgb)
            cat("\tType: RGB, 8-bit per color\n")
        else
            cat(paste("\tType: grayscale, double precision\n"))
        partial = rep(FALSE, 3)
        dmax = c(10, 10, 2)
        for(j in 1:3)
          if (d[j] > dmax[j]) {
            d[j] = dmax[j]
            partial[j] = TRUE
          }
        if(any(partial))
          cat("\tShowing ")
        if(any(partial[1:2]))
          cat(sprintf("rows 1:%d and columns 1:%d of ", d[1], d[2]))
        if(partial[3])
          cat(sprintf("images 1:%d\n", d[3]))
        if(any(partial))
          cat("\n")

        print(object@.Data[1:d[1], 1:d[2], 1:d[3]], digits=3)
#        if (!object@rgb)
#            print(summary(as.numeric(object@.Data)))
        invisible(NULL)
    }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
