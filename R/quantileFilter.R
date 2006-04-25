## Experimental quantile filter
##  (c) 2006 W. Huber
##
## Ideally, the quantile should be calculated for a circular window of radius 'radius' around
##   every pixel.
## For performance, we only calculate it for a regular grid of pixels, and then smoothly
##   interpolate to obtain values for the whole image.
##
quantileFilter = function(x, radius=min(dim(x))/8, gridStep=radius/4) {
  EBImage:::.notImageError(x)
  if (x@rgb)
    stop("'quantileFilter' requires grayscale images.")

  ## fixme - once class revision is done, need to test for dim(x)[3]==1 instead
  if(length(dim(x))!=2)
    stop("'quantileFilter' requires 2D images.")

  getGrid = function(n, w) {
    stopifnot(length(n)==1, length(w)==1)
    ngrid  = floor(n/w)
    border = (n-ngrid*w)/2
    border + (0:ngrid)*w
  }
  
  xmid = getGrid(dim(x)[1], radius)
  ymid = getGrid(dim(x)[2], radius)

  quants = matrix(as.numeric(NA), nrow=length(xmid), ncol=length(ymid))
  for(i in seq(along=xmid))
    for(j in seq(along=ymid)) {
      sel = ((col(x)-xmid)^2 + (row(x)-ymid)^2) <= radius^2
      print(sum(sel))
    }
    

}
