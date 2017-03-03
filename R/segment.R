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
  if(!is.integer(x)) storage.mode(x) = 'integer'
  y = .Call(C_ocontour, x)
  names(y) = seq_along(y)
  y = y[sapply(y, length)>0] # remove missing objects
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bwlabel = function(x) {
  validImage(x)
  .Call(C_bwlabel, x)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
colorLabels = function(x, normalize = TRUE){
  len = length( (d = dim(x)) )
  
  res <- x
  
  # linearize image data for convenient processing
  dim(res) = c(prod(d[1:2]), if(len>2) prod(d[3:len]) else 1)
  
  f = function(y, m) {
    idx <- y > 0
    y[idx] <- sample(m)[y[idx]]
    y
  }
  
  tmp = apply(res, 2, function(y) {
    m = max(y)
    replicate(3, f(y, m))
  })
  
  # restore proper dimensions
  dim(tmp) = c(d[1:2], 3, if(len>2) d[3:len] else NULL)
  
  if ( is.Image(x) ) {
    imageData(res) <- tmp
    colorMode(res) <- Color
  }
  else {
    res = new("Image", .Data = tmp, colormode = Color)
  }
  
  if (normalize) normalize(res) else res
}
