## ------------------------------------------------------------
setClass("Image2D",
    contains = "Image"
)
## ------------------------------------------------------------
setClass("Image3D",
    contains = "Image"
)
## ------------------------------------------------------------
Image2D <- function(data = array(0, c(1, 1, 1)), dim, rgb = FALSE) {
    warning("Image2D(...) is deprecated. Use Image(...) instead")
    Image(data, dim, rgb)
}
## ------------------------------------------------------------
Image3D <- function(data = array(0, c(1, 1, 1)), dim, rgb = FALSE) {
    warning("Image3D(...) is deprecated. Use Image(...) instead")
    Image(data, dim, rgb)
}
## ------------------------------------------------------------
copyImage <- function(x) {
    warning("copyImage(x) is deprecated. Use copy(...) instead")
    return(copy(x))
}
## ------------------------------------------------------------
setGeneric("minMax", function(object) standardGeneric("minMax"))
setMethod("minMax", signature(object = "Image"),
    function(object) {
        warning("minMax(object) is deprecated. Use range(object) instead")
        if (object@rgb)
            stop("Function supports grayscale images only")
        return(range(object@.Data))
    }
)
## ------------------------------------------------------------
adaptThresh <- function(x, width = 20, height = 20, offset = 0.05, preprocess = FALSE) {
    warning("adaptThresh(...) is deprecated. Use thresh(...) instead. Argument offset has now different range.")
    thresh(x, width, height, offset, preprocess)
}
## ------------------------------------------------------------
scaleImage <- function(x, dx, dy) {
    warning("scaleImage() and sampleImage(...) are deprecated. Use sample.image(...) instead")
    sample.image(x, dx, dy)
}
## ------------------------------------------------------------
sampleImage <- function(x, dx, dy) {
    warning("scaleImage() and sampleImage(...) are deprecated. Use sample.image(...) instead")
    sample.image(x, dx, dy)
}
