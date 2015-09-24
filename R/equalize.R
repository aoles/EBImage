
equalize <- function(x, range = c(0, 1), levels = 256){
  validImage(x)
  r = range(x)
  if ( r[1L] == r[2L] ) {
    warning("Image histogram is single-valued and cannot be equalized.")
  }
  else {
    if ( !is.numeric(range) || length(range) != 2L ) stop("'range' must be a numeric vector of length 2.")
    levels = as.integer(levels)
    if ( is.na(levels) || levels < 1L ) stop("Levels must be at least equal 1.")
    breaks = seq(range[1L], range[2L], length.out = levels + 1L)
    x = castImage(x)  # converts logical to numeric
    d = dim(x)
    n = d[1L] * d[2L]
    
    # equalize each frame separately
    .equalize = function(y) {
      h = hist.default(y, breaks = breaks, plot = FALSE)
      cdf = cumsum(h$counts)
      cdf_min = min(cdf[cdf>0])
      
      equalized = ( (cdf - cdf_min) / (prod(dim(y)) - cdf_min) * (range[2L] - range[1L]) ) + range[1L]
      bins = round( (y - range[1L]) / (range[2L] - range[1L]) * (levels-1L) ) + 1L
      
      res = equalized[bins]
    }
    
    res = if ( (ld = length(d)) > 2L ) apply(x, 3L:ld, .equalize)
    else .equalize(x)
    
    # setup output image
    dim(res) = d
    imageData(x) = res
  }
  x
}
