medianFilter = function(x, size, cacheSize=512L) {
  if (size < 1) stop("'filter width must be >= 1'")
  if (size > min(dim(x)[1:2]/2-1)) stop("'filter width must be less than half of the width of the x or y dimension'")
  if (any(is.na(x))) stop("'x' shouldn't contain any NAs")
  validImage(x)
  x=castImage(x)
  
  .Call(C_medianFilter, x, as.integer(size), as.integer(cacheSize))
}
