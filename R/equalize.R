
equalize <- function(x, range = c(0, 1), levels = 256){
  validImage(x)
  #if ( colorMode(x) != Grayscale ) stop("Only thresholding of Grayscale images is supported.")
  if ( !is.numeric(range) || length(range) != 2 ) stop("'range' must be a numeric vector of length 2.")
  levels = as.integer(levels)
  if ( is.na(levels) || levels < 1 ) stop("Levels must be at least equal 1.") 
  offset = (range[2]-range[1]) / (levels-1) *.5
  breaks = seq(range[1]-offset, range[2]+offset, length.out = levels+1)
  d = dim(x)
  n = prod(d[seq_len(2)])
  
# threshold each frame separately
  res = apply(x, 3:length(dim(x)), function(y) {
    h = hist.default(y, breaks = breaks, plot = FALSE)
    cdf = cumsum(h$counts)
    cdf_min = min(cdf[cdf>0])
    
    equalized = ( (cdf - cdf_min) / (prod(dim(y)) - cdf_min) * (range[2] - range[1]) ) + range[1]
    bins = round( (y - range[1]) / (range[2] - range[1]) * (levels-1) ) + 1

    res = equalized[bins]
  })

# setup output image
  dim(res) = dim(x)
  imageData(x) = res
  x
}
