normalizeImage = function(object, separate = TRUE, ft = c(0, 1), inputRange) {
  validImage(object)
  
  if ( missing(inputRange) ) {
    inputRange = NULL;
  }
  
  if ( !is.null(inputRange) ) {
    inputRange <- as.numeric(inputRange)
    if ( diff(inputRange) == 0 ) stop("clipping range is 0")
    separate = FALSE
  }
   
  if ( !is.null(ft)) {
    ft <- as.numeric (ft)
    if ( diff(ft) == 0 ) stop("normalization range is 0")
  }
  
  if ( is.null(ft) && is.null(inputRange) )
    stop("please specify either normalization range or clipping range")
  
  .Call(C_normalize, castImage(object), isTRUE(separate), ft, inputRange)
}

## general method for the superclass of 'Image' and 'matrix'
setMethod("normalize", signature("array"), normalizeImage)

## explicit methods for subclasses of 'array'
setMethod("normalize", signature("matrix"), normalizeImage)
setMethod("normalize", signature("Image"), normalizeImage)
