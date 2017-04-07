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
    d[1:2] = as.integer(round(output.dim))
  }
  
  ## inverse of the transformation matrix
  ## this is needed because in C we iterate over (x',y') for which we calculate (x,y)
  m <- solve(cbind(m, c(0, 0, 1)))
  
  ## image background
  nf = numberOfFrames(x, "render")
  
  bg <- Image(rep_len(bg.col, nf), c(1L, 1L, d[-c(1,2)]), colorMode(x))
  
  .Call(C_affine, castImage(x), d, castImage(bg), m, filter, isTRUE(antialias))
}

# images are represented in a cooridinate system with the y-axis oriented
# downwards; therefore we perform "counter-clockwise" rotation, which is
# equivalent to clockwise rotation in a regular coordinate system
rotate = function(x, angle, filter = "bilinear", output.dim, output.origin, ...) {
  ## check arguments
  if ( length(angle)!=1L || !is.numeric(angle) ) stop("'angle' must be a number")
  if ( !missing(output.dim) )
    if ( length(output.dim)!=2L || !is.numeric(output.dim) ) stop("'output.dim' must be a numeric vector of length 2")

  ## allow lossless rotation
  if ( (angle %% 90) == 0 ) filter = "none"
  
  angle = angle * pi / 180
  d  = dim(x)[1:2]
  dx = d[1]
  dy = d[2]
  cos = cos(angle)
  sin = sin(angle)
  
  ## if no origin is specified, rotate about center of output image
  if ( missing(output.origin) ) {
    ## calculate new bounding box size
    newdim = c(
      dx * abs(cos) + dy * abs(sin),
      dx * abs(sin) + dy * abs(cos)
    )
    
    ## offset needed to stay within the bounding box
    offset = c(
      dx * max(0, -cos) + dy * max(0,  sin),
      dx * max(0, -sin) + dy * max(0, -cos)
    )
    
    if ( missing(output.dim) )
      output.dim = newdim
    else
      offset = offset + (output.dim - newdim) / 2
    
  } else {
    if ( length(output.origin)!=2L || !is.numeric(output.origin) )
      stop("'output.origin' must be a numeric vector of length 2")
    offset = c(
      output.origin[1L] * (1 - cos) + output.origin[2L] * sin,
      output.origin[2L] * (1 - cos) - output.origin[1L] * sin
    )
  }
  
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

resize <- function(x, w, h, output.dim = c(w, h), output.origin = c(0, 0), antialias = FALSE, ...) {
  ## check arguments
  if ( missing(h) && missing(w) ) stop("either 'w' or 'h' must be specified")
  if ( length(output.origin)!=2L || !is.numeric(output.origin) ) stop("'output.origin' must be a numeric vector of length 2")
  
  d = dim(x)[1:2]
  if ( missing(w) ) w <- round(h*d[1]/d[2])
  if ( missing(h) ) h <- round(w*d[2]/d[1])
  if ( missing(output.dim) ) output.dim <- c(w, h)
  ratio <- c(w, h)/d
  
  m <- matrix(c(ratio[1], 0, (1-ratio[1]) * output.origin[1],
                0, ratio[2], (1-ratio[2]) * output.origin[2]), 3L, 2L)
  
  affine(x = x, m = m, output.dim = output.dim, antialias = antialias, ...)
}

## transpose spatial dimensions
transpose <- function(x) {
    .Call(C_transpose, x)
}
