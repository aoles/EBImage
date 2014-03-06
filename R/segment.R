## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
watershed = function (x, tolerance=1, ext=1) {
  validImage(x)
  tolerance = as.numeric(tolerance)
  if (tolerance<0) stop( "'tolerance' must be non-negative" )
  ext = as.integer(ext)
  if (ext<1) stop( "'ext' must be a positive integer" )
  .Call("watershed", castImage(x), tolerance, ext, PACKAGE='EBImage')
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

  return(.Call( "propagate", castImage(x), castImage(seeds), mask, lambda, PACKAGE='EBImage'))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ocontour = function(x) {
  validImage(x)
  storage.mode(x)='integer'
  y = .Call('ocontour', x, PACKAGE='EBImage')[-1]
  y = lapply(y, function(z) matrix(z, ncol=2, byrow=TRUE))
  names(y) = seq(along=y)
  y[sapply(y, nrow)>0]
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bwlabel = function(x) {
  validImage(x)
  .Call("bwlabel", castImage(x), PACKAGE='EBImage')
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
  
  y = Image(data = apply(x, 2, function(y) {
                          m = max(y)
                          replicate(3, f(y, m))
                        }), 
            dim = c(d[1:2], 3, if(len>2) d[3:len] else NULL),
            colormode = "Color")
  
  if (normalize) 
    y = normalize(y)
  
  y
}


colorLabelsOld <- function(x, normalize = TRUE) {
  M <- max(x)
  R <- sample(M)
  G <- sample(M)
  B <- sample(M)
  ch1 = x
  ch2 = x
  ch3 = x
  ch1[ch1 > 0] <- R[ch1[ch1>0]]
  ch2[ch2 > 0] <- G[ch2[ch2>0]]
  ch3[ch3 > 0] <- B[ch3[ch3>0]]
  Img = Image(data = combine(ch1, ch2, ch3), colormode="Color")
  if (normalize) {
    Img <- normalize(Img)
  }
  Img
}
