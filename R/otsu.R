
otsu <- function(x, levels = 256){
  validImage(x)
  if (colorMode(x) != Grayscale) stop("Only thresholding of Grayscale images is supported.")
  levels = as.integer(levels)
  if (levels < 2 ) stop("Levels must be at least equal 2.")
  
# prepare 3D array for apply function 
  dim(x) = c(dim(x)[seq_len(2)], getNumberOfFrames(x, 'total'))
  
# threshold each frame separately
  apply(x, 3, function(y) {
    h = hist.default(y, breaks = seq(0, 1, length.out = levels+1), plot = FALSE)
    counts = as.double(h$counts)
    mids = as.double(h$mids)
    len = length(counts)
    w1 = cumsum(counts)
    w2 = w1[len] - w1
    m1 = cumsum(counts * mids)
    m2 = m1[len] - m1
    var = w1 * w2 * (m2/w2 - m1/w1)^2
    mids[which.max(var)]
  })
}
