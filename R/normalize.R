normalizeImage = function(object, separate = TRUE, ft = c(0, 1), inputRange) {
  validImage(object)
  
  ## if set to NULL only clip image
  if(is.null(ft)) {
    ft = c(0, 0)
  }
  else {
    ft <- as.numeric (ft)
    if ( diff(ft) == 0 ) stop("normalization range is 0")
  }
  if(missing(inputRange)) {
    if( all(ft==c(0,0)) ) stop("please specify either normalization range or clipping range")
    else inputRange = c(0, 0)
  }
  else {
    inputRange <- as.numeric(inputRange)
    if ( diff(inputRange) == 0 ) stop("specified clipping range is 0")
  }
  
  .Call("normalize", castImage(object), as.integer(separate), ft, inputRange, PACKAGE='EBImage')
}

## general method for the superclass of 'Image' and 'matrix'
setMethod("normalize", signature("array"), normalizeImage)

## explicit methods for subclasses of 'array'
setMethod("normalize", signature("matrix"), normalizeImage)
setMethod("normalize", signature("Image"), normalizeImage)
