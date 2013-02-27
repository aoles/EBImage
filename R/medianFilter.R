medianFilter = function(x, size, cacheSize=512) {
 if (size <= 1) stop("'filter width must be >1'")
 if (size > min(dim(x)[1:2]/2-1)) stop("'filter width must be lesser than half of the width of the x or y dimension'")
 if (any(is.na(x))) stop("'x' shouldn't contain any NAs")
 validImage(x)
 colMode=colorMode(x)
 # clip image data to [0,1] range and covert to integer range
 x = clipImage(x)
 x=x*(2^16-1)
 x=castImage(x)
 trueDim=dim(x)
 effDimLength=length(dim(x))-ifelse(colorMode(x)==2,1,0)
 if (effDimLength >3) {stop("'x' should have <=3 dimensions")}
 else if (effDimLength==3) {
  x=apply(x, 3, function(y) {.Call('medianFilter', y, size, colorMode(x)+1, cacheSize)})
  dim(x)=trueDim
  x=as.Image(x)
 }
 else {
  x=as.Image(.Call('medianFilter', x, size, colorMode(x)+1, cacheSize))
 }
 colorMode(x)=colMode
 # map back to [0,1] range
 return(x/(2^16-1))
}
