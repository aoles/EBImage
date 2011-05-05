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
thresh = function (x, w=5, h=5, offset=0.01) {
  validImage(x)
  if ( colorMode(x) == TrueColor )
    stop ("'thresh' doesn't support the \'TrueColor\' color mode, use the \'Color\' mode instead or 'athresh'")
  if ( w < 2 || h < 2 )
    stop ("width 'w' and height 'h' must be larger than 1")
  return ( .Call("thresh", castImage(x), as.numeric( c(w, h, offset) ), PACKAGE='EBImage') )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
distmap = function (x, metric=c('euclidean', 'manhattan')) {
  validImage(x)
  if (colorMode(x)==TrueColor) stop("this method doesn't support the \'TrueColor\' color mode")
  if (any(is.na(x))) stop("'x' shouldn't contain any NAs")
  metric=match.arg(metric)
  imetric=switch(metric,euclidean=0,manhattan=1)
  return (.Call("distmap", castImage(x), as.integer(imetric), PACKAGE='EBImage'))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
makeBrush = function(size, shape=c('box', 'disc', 'diamond', 'gaussian'), step=TRUE, sigma=0.3) {
  if(! (is.numeric(size) && (length(size)==1L) && (size>=1)) ) stop("'size' must be a numeric of length 1 with value >=1.")
  shape=match.arg(shape)
  
  if (shape=='box') z = array(1,dim=c(size,size))
  else if (shape=='gaussian') {
    x = seq(-(size-1)/2, (size-1)/2, length=size)
    x = matrix(x, nrow=size, ncol=size)
    z = exp(- (x^2 + t(x)^2) / (2*sigma^2))
    z = z / sum(z)
  } else {
    ## pixel center coordinates
    x = 1:size -((size+1)/2)
    
    ## for each pixel, compute the distance from its center to the origin, using L1 norm ('diamond') or L2 norm ('disc')
    if (shape=='disc') {
      z = outer(x, x, FUN=function(X,Y) (X*X+Y*Y))
      mz = (size/2)^2
      z = (mz - z)/mz
      z = sqrt(ifelse(z>0, z, 0))
    } else {
      z = outer(x, x, FUN=function(X,Y) (abs(X)+abs(Y)))
      mz = (size/2)
      z = (mz - z)/mz
      z = ifelse(z>0, z, 0)
    }

    if (step) z = ifelse(z>0, 1, 0)
  }
  z
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
erode = function (x, kern=makeBrush(5, shape='diamond'), iter) {
  validImage(x)
  if (colorMode(x)==TrueColor) stop("this method doesn't support the \'TrueColor\' color mode")
  if (!missing(iter)) warning("'iter' is a deprecated argument.")
  return (.Call("lib_erode_dilate", castImage(x), kern, as.integer(0), PACKAGE='EBImage') )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dilate = function (x, kern=makeBrush(5, shape='diamond'), iter) {
  validImage(x)
  if (colorMode(x)==TrueColor) stop("this method doesn't support the \'TrueColor\' color mode")
  if (!missing(iter)) warning("'iter' is a deprecated argument.")  
  return (.Call("lib_erode_dilate", castImage(x), kern, as.integer(1), PACKAGE='EBImage') )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
opening = function (x, kern=makeBrush(5, shape='diamond'), iter) {
  validImage(x)
  if (!missing(iter)) warning("'iter' is a deprecated argument.")  
  dilate(erode(x, kern), kern)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
closing = function (x, kern=makeBrush(5, shape='diamond'), iter) {
  validImage(x)
  if (!missing(iter)) warning("'iter' is a deprecated argument.")  
  erode(dilate(x, kern), kern)
}
