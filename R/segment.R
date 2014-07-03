## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
watershed = function (x, tolerance=1, ext=1) {
  validImage(x)
  tolerance = as.numeric(tolerance)
  if (tolerance<0) stop( "'tolerance' must be non-negative" )
  ext = as.integer(ext)
  if (ext<1) stop( "'ext' must be a positive integer" )
  .Call(C_watershed, castImage(x), tolerance, ext)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
propagate = function (x, seeds, mask=NULL, lambda=1e-4) {
  validImage(x)
  checkCompatibleImages(x, seeds)

  if (!is.null(mask)) {
    checkCompatibleImages(x, mask)
    mask = castImage(mask)
  }

  lambda = as.numeric(lambda)
  if (lambda<0.0) stop("'lambda' must be positive" )

  return(.Call(C_propagate, castImage(x), castImage(seeds), mask, lambda))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ocontour = function(x) {
  validImage(x)
  storage.mode(x)='integer'
  y = .Call(C_ocontour, x)[-1]
  y = lapply(y, function(z) matrix(z, ncol=2, byrow=TRUE))
  names(y) = seq_along(y)
  y[sapply(y, nrow)>0]
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bwlabel = function(x) {
  validImage(x)
  .Call(C_bwlabel, castImage(x))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
colorLabels = function(x, normalize = TRUE){
  len = length( (d = dim(x)) )
  x = imageData(x)
  
  # linearize image data for convenient processing
  dim(x) = c(prod(d[1:2]), if(len>2) prod(d[3:len]) else 1)
  
  f = function(y, m) {
    y[y > 0] = sample(m)[y[y > 0]]
    y
  }
  
  y = apply(x, 2, function(y) {
    m = max(y)
    replicate(3, f(y, m))
  })
  
  # restore proper dimensions
  dim(y) = c(d[1:2], 3, if(len>2) d[3:len] else NULL)
  
  y = new("Image", .Data = y, colormode = Color)
  
  if (normalize) 
    y = normalize(y)
  
  y
}
