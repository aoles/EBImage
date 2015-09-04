drawCircle <- function(img, x, y, radius, col, fill=FALSE, z=1) {
  validImage(img)
  if ( any(is.na(img)) ) stop("'img' shouldn't contain any NAs")

  toScalarInteger = function(x) suppressWarnings(as.integer(x)[1L])
  
  ## check whether parameters are OK
  r = toScalarInteger(radius)
  if ( !isTRUE(r>0L) ) stop("'radius' must be a positive integer")
  
  z = toScalarInteger(z)
  if ( !isTRUE(z>0L) || isTRUE(z>.numberOfFrames(img, 'render')) ) stop("'z' must be a positive integer lower than the number of image frames")
  
  xy = c(toScalarInteger(x), toScalarInteger(y))
  if ( any(is.na(xy)) ) stop("'x', 'y' must be numeric scalars")
  
  fill = suppressWarnings(as.integer(isTRUE(as.logical(fill)[1L])))
  
  if (colorMode(img)==Color) {
    rgb = as.numeric(col2rgb(col)/255)
    if (length(rgb)!=3 || any(is.na(rgb))) stop("In Color mode, 'col' must be a valid color")
  } else {
    rgb = as.numeric(c(col, 0, 0))
    if (length(rgb)!=3 || any(is.na(rgb))) stop("In Grayscale mode, 'col' must be a scalar value")
  }
  
  invisible(.Call(C_drawCircle, castImage(img), c(xy-1L, z-1L, r), rgb, fill))
}
