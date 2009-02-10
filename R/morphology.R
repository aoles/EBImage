# Image morphology filters

# Copyright (c) 2005 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## half width and height: moving frame will be 2 times + 1 px larger
setMethod ("thresh", signature(x="array"),
    function (x, w=5, h=5, offset=0.01) {
        if ( colorMode(x) == TrueColor )
            stop ( .("'thresh' doesn't support the \'TrueColor\' color mode, use the \'Color\' mode instead or 'athresh'") )
        if ( w < 2 || h < 2 )
            stop ( .("width 'w' and height 'h' must be larger than 1") )
        return ( .ImageCall("thresh", x, as.numeric( c(w, h, offset) ) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("distmap", signature(x="array"),
  function (x, metric=c("euclidean",'manhattan')) {
    if (colorMode(x)==TrueColor) stop("this method doesn't support the \'TrueColor\' color mode")
    if (any(is.na(x))) stop("'x' shouldn't contain any NAs")
    metric=match.arg(metric)
    imetric=switch(metric,euclidean=0,manhattan=1)
    return (.ImageCall("distmap", x, as.integer(imetric)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
morphKern <- function (size=5, shape="round") {
    if ( size < 3 || ( size / 2 == as.integer(size / 2) ) )
        stop ( .("kernel size must be an odd number >= 3: [3, 5, 7, ...") )
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
    # otherwise square
    res[] = 1
    return(res)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mkball = function(n, shape="step") {
  if(! (is.numeric(n) && (length(n)==1L) && (n>=1)) )
    stop("'n' must be a numeric of length 1 with value >=1.")

  ## pixel center coordinates
  x = 1:n -((n+1)/2)
  
  ## for each pixel, compute the distance from its center to the origin
  d = outer(x, x, FUN=function(X,Y) (X*X+Y*Y))

  ## radius and z^2
  rsq = (n%/%2)^2
  z2 = (rsq - d)
  
  switch(shape,
         step = ifelse(z2>=0, 1, 0),
         ball = sqrt(ifelse(z2>0, z2, 0)),
         stop(sprintf("Invalid 'shape': %s", shape))
  )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mkbox = function(n) {
  matrix(1.0/(n*n),nc=n,nr=n)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("erode", signature(x="array"),
    function (x, kern=morphKern(5), iter=1) {
      if (colorMode(x)==TrueColor) stop("this method doesn't support the \'TrueColor\' color mode")
      if ( iter < 1 ) stop ( .("'iter' must be a positive integer") )
      return ( .ImageCall("lib_erode_dilate", x, kern, as.integer(iter), as.integer(0) ) )
     }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("dilate", signature(x="array"),
    function (x, kern=morphKern(5), iter=1) {
      if (colorMode(x)==TrueColor) stop("this method doesn't support the \'TrueColor\' color mode")
      if ( iter < 1 ) stop ( .("'iter' is assumed to be a positive integer") )    
      return ( .ImageCall("lib_erode_dilate", x, kern, as.integer(iter), as.integer(1) ) )
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("opening", signature(x="array"),
    function (x, kern=morphKern(5), iter=1) {
      y=erode(x, kern, iter)
      dilate (y, kern, iter)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("closing", signature(x="array"),
    function (x, kern=morphKern(5), iter=1) {
      y=dilate(x, kern, iter)
      erode (y, kern, iter)
    }
)
