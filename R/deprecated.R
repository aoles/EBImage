setMethod ("write.image", signature(x="Image"),
  function (x, ...) {
    .Deprecated("writeImage", "EBImage")
    writeImage(x, ...)
  }
)

read.image <- function(files, colormode=Grayscale, ...) {
  .Deprecated("readImage", "EBImage")
  readImage(files, colormode, ...)
}

choose.image <- function(colormode=Grayscale) {
  .Deprecated("chooseImage", "EBImage")
  chooseImage(colormode)
}

setMethod ("hull.features", signature(x="ImageX"),
  function(x, ...) {
    .Deprecated("hullFeatures", "EBImage")
    hullFeatures(x, ...)
  }
)

setMethod ("edge.profile", signature(x="ImageX"),
  function (x, ...) {
    .Deprecated("edgeProfile", "EBImage")
    edgeProfile(x, ...)
  }
)

setMethod ("edge.features", signature(x="ImageX"),
  function (x, ...) {
    .Deprecated("edgeFeatures", "EBImage")
    edgeFeatures(x, ...)
  }
)

setMethod ("haralick.matrix", signature(x="ImageX", ref="ImageX"),
  function(x, ref, ...) {
    .Deprecated("haralickMatrix", "EBImage")
    haralickMatrix(x, ref, ...)
  }
)

setMethod ("haralick.features", signature(x="ImageX", ref="ImageX"),
  function(x, ref, ...) {
    .Deprecated("haralickFeatures", "EBImage")
    haralickFeatures(x, ref, ...)
  }
)

setMethod ("zernike.moments", signature(x="ImageX", ref="ImageX"),
  function(x, ref, ...) {
    .Deprecated("zernikeMoments", "EBImage")
    zernikeMoments(x, ref, ...)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
setMethod("frameDist", signature(x="ImageX",y="ImageX"),
  function(x, y, r, g, b, blur=TRUE, method="dist", verbose, ...) {
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
        m = morphKern(3); m[2,2] = 4; m = m/sum(m)
        x = filter2(x, m)
        y = filter2(y, m)
      } else {
        x = blur(x, 1.5, 1.0)
        y = blur(y, 1.5, 1.0)
      }
    }
    return(.Call("lib_frameDist", x, y, weights, method, as.integer(verbose)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
setMethod("frameDist", signature(x="Image",y="missing"),
  function(x, y, r, g, b, blur=TRUE, method="dist", verbose, ...) {
    if (missing(verbose)) verbose = options()$verbose
    if (colorMode(x)==Grayscale && (!missing(r)||!missing(g)||!missing(b)))
      warning("r, g, b are used only for TrueColor images")
    if (missing(r)) r = 1.0
    if (missing(g)) g = 1.0
    if (missing(b)) b = 1.0
    method = as.integer(switch(tolower(substr(method,1,3)), dis=0, dot=1, cor=2))
    weights = as.double(c(r,g,b,0.0))
    if (blur) {
      if (colorMode(x)==Grayscale) {
        m = morphKern(3); m[2,2] = 4; m = m/sum(m)
        x = filter2(x, m)
      } else x = blur(x, 1.5, 1.0)
    }
    return(.Call("lib_frameDist", x, x, weights, method, as.integer(verbose)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("matchObjects", signature(x="ImageX", ref="ImageX"),
  function (x, ref, ...) {
    if ( colorMode(x) == TrueColor ) stop("'x' must be an Image not in \'TrueColor\' color mode")
   
    if ( !assert(x, ref, strict=TRUE) )
      .stop( "dim(x) must equal dim(ref), 'ref' must be Grayscale" )
    return ( .ImageCall ("matchObjects", x, ref) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stopIfNotImage <- function (x) {
  if ( !is.Image(x) ) stop( "argument must be of class 'Image'" )
  invisible (NULL)
}
