drawCircle <- function(img, x, y, radius, col, fill=FALSE, z=1) {
  validImage(img)
  if (any(is.na(img))) stop("'x' shouldn't contain any NAs")

  ## check whether parameters are OK
  if (missing(radius)) stop("'radius' is missing")
  if (radius<1) stop("'radius' must be positive integer")
  if (z<1 | z>getNumberOfFrames(img, 'render')) stop("'z' must be a positive integer lower than the number of image frames")
  xyzr = as.integer(c(x, y, z-1, radius))
  if (length(xyzr)!=4 || any(is.na(xyzr))) stop("'x', 'y', 'z' and 'radius' must be scalar values")
  fill = as.integer(fill)
  if (length(fill)!=1)  stop("'fill' must be a logical")
  
  if (colorMode(img)==Color) {
    rgb = as.numeric(col2rgb(col)/255)
    if (length(rgb)!=3 || any(is.na(rgb))) stop("In Color mode, 'col' must be a valid color")
  } else {
    rgb = as.numeric(c(col, 0, 0))
    if (length(rgb)!=3 || any(is.na(rgb))) stop("In Grayscale mode, 'col' must be a scalar value")
  }
  
  invisible(.Call(C_drawCircle, castImage(img), xyzr, rgb, fill))
}
