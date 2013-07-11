normalize.Image = function(object, separate = TRUE, ft = c(0, 1), inputRange) {
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

setMethod("normalize", signature("Image"), normalize.Image)
setMethod("normalize", signature("array"), normalize.Image)
setMethod("normalize", signature("matrix"), normalize.Image)
