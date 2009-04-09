## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
watershed = function (x, tolerance=1, ext=1) {
  validImage(x)
  if ( colorMode(x) == TrueColor ) stop("'x' must be an Image not in \'TrueColor\' color mode")
  tolerance <- as.numeric (tolerance)
  if ( tolerance < 0 )
      stop( "'tolerance' must be non-negative" )
  ext <- as.integer (ext)
  if ( ext < 1 )
    stop( "'ext' must be a positive integer" )
  return( .Call("watershed", castImage(x), tolerance, ext, PACKAGE='EBImage') )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
propagate = function (x, seeds, mask=NULL, lambda=0.1, ext=1, seed.centers=FALSE) {
  validImage(x)
  if ( colorMode(x) == TrueColor ) stop("'x' must be an Image not in \'TrueColor\' color mode")
  
  checkCompatibleImages(x,seeds)
  if (!is.null(mask)) checkCompatibleImages(x,mask)
  
  ext <- as.integer (ext)
  if ( ext < 0 )
    stop("'ext' must be non-negative" )
  lambda <- as.numeric (lambda)
  if ( lambda < 0.0 )
    stop("'lambda' must be non-negative" )
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
  return( .Call( "lib_propagate", castImage(x), seeds, mask, ext, lambda, PACKAGE='EBImage') )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ocontour = function(x) {
  validImage(x)
  storage.mode(x)='integer'
  y = .Call('ocontour', x, PACKAGE='EBImage')[-1]
  y = lapply(y, function(z) matrix(z, nc=2, byrow=TRUE))
  y
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bwlabel = function(x) {
  validImage(x)
  return(.Call("bwlabel", castImage(x), PACKAGE='EBImage'))
}
