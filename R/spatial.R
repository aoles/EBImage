flip <- function (x) {
  validImage(x)
  nd=as.list(rep(T, length(dim(x))))
  nd[[2]]=dim(x)[2]:1
  do.call('[', c(list(x),nd))
}

flop <- function (x) {
  validImage(x)
  nd=as.list(rep(T, length(dim(x))))
  nd[[1]]=dim(x)[1]:1
  do.call('[', c(list(x),nd))
}

## performs an affine transform on a set of images
affine <- function (x, m, filter=c("bilinear", "none"), output.dim) {
  ## check arguments
  validImage(x)
  if (!is.matrix(m) || nrow(m)!=3 || ncol(m)!=2) stop("'m' must be a 3x2 matrix")
  if (any(is.na(m))) stop("'m' shouldn't contain any NAs")
  m <- cbind(m, c(0, 0, 1))
  filter <- match.arg(filter)
  filter <- as.integer(c(none=0, bilinear=1)[filter])
  if (!missing(output.dim) && (length(output.dim)!=2 || !is.numeric(output.dim))) stop("'output.dim' must be a numeric vector of length 2")
  
  ## backtransform
  m <- solve(m)

  ## output image
  if (missing(output.dim)) {
    y <- Image(0, dim=dim(x), colormode=colorMode(x))
  } else {
    y <- Image(0, dim=c(output.dim[1], output.dim[2], tail(dim(x), -2)), colormode=colorMode(x))
  }
  
  return (.Call("affine", castImage(x), castImage(y), m, filter, PACKAGE='EBImage'))
}

rotate <- function(x, angle, filter="bilinear", output.dim, output.origin=c(0, 0)) {
  ## check arguments
  if (length(angle)!=1 || !is.numeric(angle)) stop("'angle' must be a number")
  if (length(output.origin)!=2 || !is.numeric(output.origin)) stop("'output.origin' must be a numeric vector of length 2")
  
  theta <- angle*pi/180
  cx <- nrow(x)/2+nrow(x)*sqrt(2)*cos(theta-pi/4-pi/2)/2 + output.origin[1]
  cy <- ncol(x)/2+ncol(x)*sqrt(2)*sin(theta-pi/4-pi/2)/2 + output.origin[2]
  m <- matrix(c(cos(theta), -sin(theta), cx,
                sin(theta), cos(theta), cy), nrow=3)
  affine(x, m, filter, output.dim=output.dim)
}

translate <- function(x, v, filter="none", output.dim) {
  ## check arguments
  if (length(v)!=2 || !is.numeric(v)) stop("'v' must be a numeric vector of length 2")
  m <- matrix(c(1, 0, -v[1], 0, 1, -v[2]), nrow=3)
  affine(x, m, filter=filter, output.dim=output.dim)
}

resize <- function(x, w, h, filter="bilinear", output.dim, output.origin=c(0, 0)) {
  ## check arguments
  if (missing(h) && missing(w))  stop("'w' or 'h' must be specified")
  if (missing(w)) w <- round(h*dim(x)[1]/dim(x)[2])
  if (missing(h)) h <- round(w*dim(x)[2]/dim(x)[1])
  if (missing(output.dim)) output.dim <- c(w, h)
  if (length(output.origin)!=2 || !is.numeric(output.origin)) stop("'output.origin' must be a numeric vector of length 2")
  
  ratio <- c(w, h)/dim(x)[1:2]
  m <-  matrix(c(ratio[1], 0, output.origin[1], 0, ratio[2], output.origin[2]), nrow=3)
  affine(x, m, filter, output.dim=output.dim)
}

## transposes the XY dimensions
transpose <- function(x, coerce = FALSE) {
  validImage(x)
  dims = 1:length(dim(x))
  dims[1:2] = c(2:1)
  y = aperm(x, dims)
  if ( (!coerce) && is.Image(x) )
    x@.Data = y
  else 
    x = y
  x
}
