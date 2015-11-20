flip <- function (x) {
  validImage(x)
  asub(x, seq.int(dim(x)[2],1), 2)
}

flop <- function (x) {
  validImage(x)
  asub(x, seq.int(dim(x)[1],1), 1)
}

## performs an affine transform on a set of images
affine <- function (x, m, filter = c("bilinear", "none"), output.dim, bg.col = "black", antialias = TRUE) {
  ## check arguments
  validImage(x)
  if ( !is.matrix(m) || dim(m)!=c(3L, 2L) ) stop("'m' must be a 3x2 matrix")
  if ( any(is.na(m)) ) stop("'m' shouldn't contain any NAs")
  
  filter <- switch(match.arg(filter), none=0L, bilinear=1L)
  
  # dimensions of output image
  d = dim(x)  
  if (!missing(output.dim)) {
    if( length(output.dim)!=2L || !is.numeric(output.dim) ) stop("'output.dim' must be a numeric vector of length 2")
    d[1:2] = round(output.dim)
  }
  
  ## inverse of the transformation matrix
  ## this is needed because in C we iterate over (x',y') for which we calculate (x,y)
  m <- solve(cbind(m, c(0, 0, 1)))
  
  ## create output image
  y <- x
  imageData(y) <- bgImage(bg.col, d, colorMode(x))
  
  .Call(C_affine, castImage(x), castImage(y), m, filter, as.integer(antialias))
}

bgImage = function(col, dim, colormode) {
  
  ## character color description
  if ( is.character(col) ) {
    col = col2rgb(col, alpha = isTRUE(dim[3L]==4L)) / 255
    
    if ( colormode==Color ) {
      res = do.call(abind, c(
        lapply(seq_along(col[,1L]), function(i) array(col[i,,drop=FALSE], dim[-3])),
        along = 2.5)
      )
      dimnames(res) = NULL
    }
    else
      res = array(data = colSums(col) / 3, dim = dim)
  }
  ## numeric case
  else {
    res = array(col, dim = dim)
  }
  res
}

rotate = function(x, angle, filter = "bilinear", output.dim, ...) {
  ## check arguments
  if ( length(angle)!=1L || !is.numeric(angle) ) stop("'angle' must be a number")
  if ( !missing(output.dim) ) if ( length(output.dim)!=2L || !is.numeric(output.dim) ) stop("'output.dim' must be a numeric vector of length 2")
  
  ## allow lossless rotation
  if ( (angle %% 90) == 0 ) filter = "none"
  
  angle = angle * pi / 180
  d  = dim(x)[1:2]
  dx = d[1]
  dy = d[2]
  cos = cos(angle)
  sin = sin(angle)
  
  ## new bounding box size
  newdim = c(
    dx * abs(cos) + dy * abs(sin),
    dx * abs(sin) + dy * abs(cos)
  )
  
  ## origin offset needed to stay in the bounding box
  offset = c(
    dx * max(0, -cos) + dy * max(0,  sin),
    dx * max(0, -sin) + dy * max(0, -cos)
  )
    
  if ( missing(output.dim) )
    output.dim = newdim
  else
    offset = offset + (output.dim - newdim) / 2

  m <- matrix(c(cos, -sin, offset[1], 
                sin,  cos, offset[2]), 3L, 2L) 
  
  affine(x = x, m = m, filter = filter, output.dim = output.dim, ...)
}

translate <- function(x, v, filter = "none", ...) {
  ## check arguments
  if ( length(v)!=2L || !is.numeric(v) ) stop("'v' must be a numeric vector of length 2")
  
  m <- matrix(c(1, 0, v[1],
                0, 1, v[2]), 3L, 2L)
  
  affine(x = x, m = m, filter = filter, ...)
}

resize <- function(x, w, h, filter = "bilinear", output.dim = c(w, h), output.origin = c(0, 0), antialias = FALSE, ...) {
  ## check arguments
  if ( missing(h) && missing(w) ) stop("either 'w' or 'h' must be specified")
  if ( length(output.origin)!=2L || !is.numeric(output.origin) ) stop("'output.origin' must be a numeric vector of length 2")
  
  d = dim(x)[1:2]
  if ( missing(w) ) w <- round(h*d[1]/d[2])
  if ( missing(h) ) h <- round(w*d[2]/d[1])
  if ( missing(output.dim) ) output.dim <- c(w, h)
  ratio <- c(w, h)/d
  
  m <- matrix(c(ratio[1], 0, output.origin[1],
                0, ratio[2], output.origin[2]), 3L, 2L)
  
  affine(x = x, m = m, filter = filter, output.dim = output.dim, antialias = antialias, ...)
}

## transposes the XY dimensions
transpose <- function(x, coerce = FALSE) {
  validImage(x)
  dims = seq_along(dim(x))
  dims[1:2] = c(2:1)
  y = aperm(x, dims)
  if ( (!coerce) && is.Image(x) ) {
    x@.Data = y
    x
  }
  else y
}

# transposeImage = function(x, coerce = FALSE) {
#   validImage(x)
#   dims = seq_along(dim(x))
#   dims[1:2] = c(2:1)
#   y = aperm(x, dims)
#   if ( (!coerce) && is.Image(x) ) {
#     x@.Data = y
#     x
#   }
#   else y
# }
# 
# transpose <- function(x, coerce = FALSE) {
#   .Deprecated("t")
#   transposeImage(x, coerce)
# }
# 
# t.Image  <- function(x) transposeImage(x)
# t.array  <- function(x) transposeImage(x)
# t.matrix <- function(x) t.default(x)
