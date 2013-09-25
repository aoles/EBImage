normalizeImage = function(object, separate = TRUE, ft = c(0, 1), inputRange) {
  validImage(object)
  ft <- as.numeric (ft)
  if ( diff(ft) == 0 ) stop("normalization range is 0")
  if(missing(inputRange)) {
    inputRange = c(0, 0)
  }
  else {
    inputRange <- as.numeric(inputRange)
    if ( diff(inputRange) == 0 ) stop("specified input range is 0")
  }
  object = .Call("normalize", castImage(object), as.integer(separate), ft, inputRange, PACKAGE='EBImage')
  return(object)
}

## general method for the superclass of 'Image' and 'matrix'
setMethod("normalize", signature("array"), normalizeImage)

## explicit methods for subclasses of 'array'
setMethod("normalize", signature("matrix"), normalizeImage)
setMethod("normalize", signature("Image"), normalizeImage)
