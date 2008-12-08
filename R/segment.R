# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("watershed", signature(x="ImageX"),
  function (x, tolerance=1, ext=1) {
    if ( colorMode(x) == TrueColor ) stop("'x' must be an Image not in \'TrueColor\' color mode")
    tolerance <- as.numeric (tolerance)
    if ( tolerance < 0 )
      .stop( "'tolerance' must be non-negative" )
    ext <- as.integer (ext)
    if ( ext < 1 )
      .stop( "'ext' must be a positive integer" )
    return( .ImageCall("watershed", x, tolerance, ext) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("propagate", signature(x="ImageX", seeds="ImageX"),
  function (x, seeds, mask=NULL, lambda=0.1, ext=1, seed.centers=FALSE) {
    if ( colorMode(x) == TrueColor ) stop("'x' must be an Image not in \'TrueColor\' color mode")
    if ( !assert(x, seeds, strict=TRUE) || (!is.null(mask) && !assert(x, mask, strict=TRUE)) )
      .stop( "dim(x) must equal dim(seeds) and dim(mask) if mask is not NULL, all images must be Grayscale" )
    ext <- as.integer (ext)
    if ( ext < 0 )
      .stop("'ext' must be non-negative" )
    lambda <- as.numeric (lambda)
    if ( lambda < 0.0 )
      .stop("'lambda' must be non-negative" )
    if (seed.centers) {
      cm = cmoments(seeds)
      dimx = dim(seeds)
      if (dimx[3]==1) cm = list(cm)
      index = lapply(cm, function(xy) {
        floor(xy[,3]) + floor(xy[,4])*dimx[1]
      })
      for (i in 1:dimx[3]) index[[i]] = index[[i]] + (i-1)*dimx[1]*dimx[2]
      index = unlist(index)
      s = seeds
      s[] = 0.0
      s[index] = seeds[index]
      seeds = s
    }
    return( .Call( "lib_propagate", x, seeds, mask, ext, lambda) )
  }
)
