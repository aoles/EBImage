
otsu <- function(x, range = c(0, 1), levels = 256){
  validImage(x)
  if ( colorMode(x) != Grayscale ) stop("Only thresholding of Grayscale images is supported.")
  if ( !is.numeric(range) || length(range) != 2 ) stop("'range' must be a numeric vector of length 2.")
  levels = as.integer(levels)
  if ( is.na(levels) || levels < 1 ) stop("Levels must be at least equal 1.") 
  breaks = seq(range[1], range[2], length.out = levels+1)
  
# prepare 3D array for apply function 
  dim(x) = c(dim(x)[seq_len(2)], numberOfFrames(x, 'total'))
  
# threshold each frame separately
  apply(x, 3, function(y) {
    h = hist.default(y, breaks = breaks, plot = FALSE)
    counts = as.double(h$counts)
    mids = as.double(h$mids)
    len = length(counts)
    w1 = cumsum(counts)
    w2 = w1[len] + counts - w1
    cm = counts * mids
    m1 = cumsum(cm)
    m2 = m1[len] + cm - m1
    var = w1 * w2 * (m2/w2 - m1/w1)^2
    # find the left- and right-most maximum and return the threshold value in between
    maxi = which(var == max(var, na.rm = TRUE))
    (mids[maxi[1]] + mids[maxi[length(maxi)]] ) /2
  })
}
