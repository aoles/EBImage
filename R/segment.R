## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
watershed = function (x, tolerance=1, ext=1) {
  validImage(x)
  if (colorMode(x)==TrueColor) stop("'x' must be an Image not in \'TrueColor\' color mode")
  tolerance = as.numeric(tolerance)
  if (tolerance < 0) stop( "'tolerance' must be non-negative" )
  ext = as.integer(ext)
  if (ext<1) stop( "'ext' must be a positive integer" )
  .Call("watershed", castImage(x), tolerance, ext, PACKAGE='EBImage')
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
propagate = function (x, seeds, mask=NULL, lambda=1e-4, ext, seed.centers) {
  validImage(x)
  checkCompatibleImages(x, seeds)
  
  if (!is.null(mask)) {
    checkCompatibleImages(x, mask)
    mask = castImage(mask)
  }

  lambda = as.numeric(lambda)
  if (lambda < 0.0) stop("'lambda' must be positive" )

  if (!missing(ext)) warning("'ext' is deprecated.")
  
  if (!missing(seed.centers)) warning("'seed.centers' is deprecated.")
  else seed.centers = FALSE
  
  if (seed.centers) {
    cm = hullFeatures(seeds)
    dimx = dim(seeds)
    nz = getNumberOfFrames(seeds, 'total')
    if (nz==1) cm = list(cm)
    index = lapply(cm, function(xy) floor(xy[,'g.x']) + floor(xy[,'g.y'])*dimx[1])
    for (i in 1:nz) index[[i]] = index[[i]] + (i-1)*dimx[1]*dimx[2]
    index = unlist(index)
    s = Image(0, dim=dim(seeds))
    s[index] = seeds[index]
    seeds = s
  }
  
  return(.Call( "propagate", castImage(x), castImage(seeds), mask, lambda, PACKAGE='EBImage'))
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
  .Call("bwlabel", castImage(x), PACKAGE='EBImage')
}
