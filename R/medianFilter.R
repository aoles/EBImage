medianFilter = function(x, size, cacheSize=512L) {
  size = as.integer(size)
  if (size < 1L) stop("filter radius must be >= 1")
  if (min(dim(x)[1:2]) < 2L * size + 1L) stop("filter radius must not exceed half of the x or y dimension")
  if (any(is.na(x))) stop("'x' shouldn't contain any NAs")
  validImage(x)
  
  .Call(C_medianFilter, castImage(x), size, as.integer(cacheSize))
}
