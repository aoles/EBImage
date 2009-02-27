write.image = function (x, ...) {
  .Deprecated("writeImage", "EBImage")
  writeImage(x, ...)
}

read.image = function(files, colormode=Grayscale, ...) {
  .Deprecated("readImage", "EBImage")
  readImage(files, colormode, ...)
}

choose.image = function(colormode=Grayscale) {
  .Deprecated("readImage", "EBImage")
  if (!missing(colormode)) warning("'colormode' is deprecated")
  else colormode=-1
  readImage(colormode=colormode)
}

hull.features = function(x, ...) {
  .Deprecated("hullFeatures", "EBImage")
  hullFeatures(x, ...)
}

edge.profile = function (x, ...) {
  .Deprecated("edgeProfile", "EBImage")
  edgeProfile(x, ...)
}

edge.features = function (x, ...) {
  .Deprecated("edgeFeatures", "EBImage")
  edgeFeatures(x, ...)
}

haralick.matrix = function(x, ref, ...) {
  .Deprecated("haralickMatrix", "EBImage")
  haralickMatrix(x, ref, ...)
}

haralick.features = function(x, ref, ...) {
  .Deprecated("haralickFeatures", "EBImage")
  haralickFeatures(x, ref, ...)
}

zernike.moments = function(x, ref, ...) {
  .Deprecated("zernikeMoments", "EBImage")
  zernikeMoments(x, ref, ...)
}

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
