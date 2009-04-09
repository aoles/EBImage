## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
frameDist = function(x, y, r, g, b, blur=TRUE, method="dist", verbose, ...) {
  .Deprecated()
  if (missing(verbose)) verbose = options()$verbose
  if (colorMode(x)!=colorMode(y))
    stop("'x' and 'y' must be in the same color mode")
  if (colorMode(x)==Grayscale && (!missing(r)||!missing(g)||!missing(b)))
    warning("r, g, b are used only for TrueColor images")
  if (missing(r)) r = 1.0
  if (missing(g)) g = 1.0
  if (missing(b)) b = 1.0
  method = as.integer(switch(tolower(substr(method,1,3)), dis=0, dot=1, cor=2))
  weights = as.double(c(r,g,b,0.0))
  if (blur) {
    if (colorMode(x)==Grayscale) {
      m = makeBrush(3,shape='diamond'); m[2,2] = 4; m = m/sum(m)
      x = filter2(x, m)
      y = filter2(y, m)
    } else {
      x = blur(x, 1.5, 1.0)
      y = blur(y, 1.5, 1.0)
    }
  }
  return(.Call("lib_frameDist", x, y, weights, method, as.integer(verbose)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
matchObjects = function (x, ref, ...) {
  .Deprecated()
  if ( colorMode(x) == TrueColor ) stop("'x' must be an Image not in \'TrueColor\' color mode")
  
  if ( !assert(x, ref, strict=TRUE) ) stop( "dim(x) must equal dim(ref), 'ref' must be Grayscale" )
  return ( .Call ("matchObjects", castImage(x), ref), PACKAGE='EBImage')
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stopIfNotImage = function (x) {
  .Deprecated()
  if ( !is.Image(x) ) stop( "argument must be of class 'Image'" )
  invisible (NULL)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
morphKern = function (size=5, shape="round") {
  .Deprecated('makeBrush')
  if ( size < 3 || ( size / 2 == as.integer(size / 2) ) )
    stop ("kernel size must be an odd number >= 3: [3, 5, 7, ...")
  if ( switch(shape, round=, square=FALSE, TRUE) )
    stop("available shapes 'round' and 'square'")
  res <- matrix (0, size, size, byrow = TRUE )
  cx = as.integer(size / 2) + 1
  if (shape == "round") {
    res[cx,] = 1
    res[,cx] = 1
    for ( i in 1:(cx-1) )
      for ( j in 1:(cx-1) )
        if ( (cx - i)^2 + (cx - j)^2 <= (cx - 1)^2 ) {
          res[i, j] = 1
          res[size - i + 1, j] = 1
          res[i, size - j + 1] = 1
          res[size - i + 1, size - j + 1] =1
        }
    return (res)
  }
  ## otherwise square
  res[] = 1
  return(res)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mkball = function(n, shape="step") {
  .Deprecated('makeBrush')
  if(! (is.numeric(n) && (length(n)==1L) && (n>=1)) )
    stop("'n' must be a numeric of length 1 with value >=1.")

  ## pixel center coordinates
  x = 1:n -((n+1)/2)
  
  ## for each pixel, compute the distance from its center to the origin
  d = outer(x, x, FUN=function(X,Y) (X*X+Y*Y))
  d = outer(x, x, FUN=function(X,Y) (abs(X)+abs(Y)))

  ## radius and z^2
  rsq = (n%/%2)^2
  rsq = n/2
  z2 = (rsq - d)
  
  switch(shape,
         step = ifelse(z2>=0, 1, 0),
         ball = sqrt(ifelse(z2>0, z2, 0)),
         stop(sprintf("Invalid 'shape': %s", shape))
  )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mkbox = function(n) {
  .Deprecated('makeBrush')
  matrix(1.0/(n*n),nc=n,nr=n)
}

## Shallow copy of the object: only the members (not the array) are copied
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
header = function (x) {
  .Deprecated()
  dim=rep(1,length(dim(x)))
  if (colorMode(x) == Grayscale) data=array(0,dim=dim)
  else if (colorMode(x) == TrueColor) data=array(0L,dim=dim) 
  else data=array(0,dim=dim)
  
  if (class(x)=='array') return(data)
  else {
    y = new(class(x),.Data=data,colormode=colorMode(x))
    return(y)
  }
}

## Assert (misnamed) checks Image dimension & color compatibility
## If strict is TRUE,  all the dimensions and colorMode are checked
## If strict is FALSE, only the two first dimensions and colorMode are checked
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
assert = function (x, y, strict=FALSE) {
  .Deprecated()
  n <- 2
  if (missing(y)) return(is.Image(x))
  if (strict) {
    if (length(dim(x))!=length(dim(y))) return(FALSE)
    else n = length(dim(x))
  }
  if ( any( dim(x)[1:n] != dim(y)[1:n] ) || colorMode(x) != colorMode(y) )
    return( FALSE )
    return( TRUE )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
chooseImage = function(colormode) {
  .Deprecated("readImage", "EBImage")
  if (!missing(colormode)) warning("'colormode' is deprecated")
  else colormode=-1
  readImage(colormode=colormode)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
resample = function (x, w, h) {
  .Deprecated("resize", "EBImage")
  if (missing(w) && missing(h))
    stop("either 'w' or 'h' must be specified")
  dimx = dim(x)
  if (missing(w)) w = dimx[1]*h / dimx[2]
  else if (missing(h)) h = dimx[2]*w / dimx[1]
  if (w <= 0 || h <= 0)
    stop("width and height of a new image must be non zero positive")
    param = as.numeric(c(w, h))
  return(ImageMagickCall(x, flt.sample, param))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sharpen = function (x, r=0, s=0.5) {
  .Deprecated("filter2", "EBImage")
  if (r <= s && r != 0)
    warning("for reasonable results, 'r' should be larger than 's'")
  if (r < 0 || s <= 0)
    stop("values of 'r' and 's' must be positive, alternatively r=0 selects radius automatically")
  param = as.numeric(c(r, s))
  return(ImageMagickCall( x, flt.sharpen, param))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
umask = function (x, r=0, s=0.5, amount=5, t=2) {
  .Deprecated("filter2", "EBImage")
  if (r <= s && r != 0)
    warning("for reasonable results, 'r' should be larger than 's'")
  if (r < 0 || s <= 0 || amount < 0 || t < 0)
    stop("all arguments must be positive, alternatively r=0 selects radius automatically")
  param = as.numeric(c(r, s, amount, t))
  return(ImageMagickCall( x, flt.unsharp, param))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
modulate = function (x, value=100) {
  .Deprecated()
  return(ImageMagickCall(x, flt.modulate, as.numeric(value)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
negate = function (x) {
  .Deprecated(paste("1-",paste(deparse(substitute(x))), sep=''))
  return(ImageMagickCall(x, flt.negate, as.numeric(0)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
affinet = function (x, sx=0, rx=0, ry=0, sy=0, tx=0, ty=0) {
  .Deprecated("resize", "EBImage")
  param = as.numeric(c(sx, rx, ry, sy, tx, ty))
  return(ImageMagickCall(x, flt.affinet, param))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
normalize2 = function (x) {
  .Deprecated("normalize", "EBImage")
  return(ImageMagickCall(x, flt.norm, as.numeric(0)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
noise = function (x, type="G") {
  .Deprecated("rnorm")
  type = tolower(substr(type,1,1))
  param = as.numeric(switch(type, u= 1, g= 2, m= 3, i= 4, l= 5, p= 6, 2))
  if (param == 2 && type != "g")
    warning("unsupported noise type, using 'gaussian' instead. Possible values: uniform, gaussian, multi, impulse, laplace and poisson")    
  return(ImageMagickCall(x, flt.noise, param))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mediansmooth = function (x, r=2) {
  .Deprecated("filter2", "EBImage")
  if (r <= 1) stop("value of 'r' must be larger than 1")      
  return(ImageMagickCall(x, flt.median, as.numeric(r)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cgamma = function (x, level=1) {
  .Deprecated(paste(paste(deparse(substitute(x))),"^level", sep=''))
  if (level < 0.8 || level > 2.3)
    warning("reasonable 'level' is between 0.8 and 2.3")     
  return(ImageMagickCall(x, flt.gamma, as.numeric(level)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
enhance = function (x) {
  .Deprecated()
  return(ImageMagickCall(x, flt.enhance, as.numeric(0)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
denoise = function (x, r=0) {
  .Deprecated("filter2", "EBImage")
  if (r < 0) stop("'r' must be positive, set r=0 for automatic selection")    
  return(ImageMagickCall(x, flt.denoise, as.numeric(r)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
contrast = function (x, sharpen=TRUE) {
  .Deprecated(paste(paste(deparse(substitute(x))),"*2", sep=''))
  return(ImageMagickCall(x, flt.contrast, as.numeric(sharpen)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
despeckle = function (x) {
  .Deprecated("filter2", "EBImage")
  return(ImageMagickCall (x, flt.despeckle, as.numeric(0)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
edge = function (x, r=0) {
  .Deprecated("filter2", "EBImage")
  return(ImageMagickCall(x, flt.edge, as.numeric(r)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
segment = function (x, cl=10, s=1.5) {
  .Deprecated("filter2", "EBImage")
  if (cl < 1) stop("cluster size 'cl' must be larger than 1")
  if (s <= 0) stop("smoothness 's' must be positive")
  param = as.numeric(c(cl, s))
  return(ImageMagickCall(x, flt.segment, param))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cthresh = function (x, threshold=0) {
  .Deprecated()
  return(ImageMagickCall(x, flt.cthresh, as.numeric(threshold)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
athresh = function (x, w=10, h=10, offset=0) {
  .Deprecated("thresh", "EBImage")
  if (w < 2 || h < 2) stop("width 'w' and height 'h' must be larger than 1")
  param = as.numeric(c(w, h, offset))
  return(ImageMagickCall(x, flt.athresh, param))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
channelMix = function(red, green, blue) {
  .Deprecated("sqrt(red^2+green^2+blue^2)", "EBImage")
  mix = list()
  if (!missing(red)) mix[["r"]] = red
  if (!missing(green)) mix[["g"]] = green
  if (!missing(blue)) mix[["b"]] = blue
  if (length(mix)==0)
    stop("at least one image must be provided")
  if (length(mix)==1) return(mix[[1]])
  res = mix[[1]]^2 + mix[[2]]^2
  if (length(mix)==3) res = res + mix[[3]]^2
  sqrt(res)
}
