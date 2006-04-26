## ------------------------------------------------------------
setClass("Image2D",
    contains = "Image"
)
## ------------------------------------------------------------
setClass("Image3D",
    contains = "Image"
)
## ------------------------------------------------------------
setGeneric("minMax", function(object) standardGeneric("minMax"))
## ------------------------------------------------------------
Image2D <- Image
Image3D <- Image
## ------------------------------------------------------------
copyImage <- function(x) {
    return(copy(x))
}
## ------------------------------------------------------------
setMethod("minMax", signature(object = "Image"),
    function(object) {
        if (object@rgb)
            stop("Function supports grayscale images only")
        return(range(object@.Data))
    }
)
## ------------------------------------------------------------
adaptThresh <- thresh