normalize.Image = function(object, separate = TRUE, ft = c(0, 1)) {
  validImage(object)
  ft <- as.numeric (ft)
  if ( diff(ft) == 0 ) stop("normalization range is 0")
  object = .Call("normalize", castImage(object), as.integer(separate), ft, PACKAGE='EBImage')
  return(object)
}

setMethod("normalize", signature("Image"), normalize.Image)
setMethod("normalize", signature("array"), normalize.Image)
setMethod("normalize", signature("matrix"), normalize.Image)
