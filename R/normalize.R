normalize <- function (x, separate=TRUE, ft=c(0, 1)) {
  validImage(x)
  ft <- as.numeric (ft)
  if ( diff(ft) == 0 ) stop("normalization range is 0")
  separate <- as.integer(separate)
  x = .Call("normalize", castImage(x), separate, ft, PACKAGE='EBImage')
  return(x)
}
