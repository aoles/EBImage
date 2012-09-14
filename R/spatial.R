# Filter methods for class Image

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

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
normalize = function (x, separate=TRUE, ft=c(0,1)) {
  validImage(x)
  ft <- as.numeric (ft)
  if ( diff(ft) == 0 ) stop("normalization range is 0")
  separate <- as.integer(separate)
  x = .Call("normalize", castImage(x), separate, ft, PACKAGE='EBImage')
  return(x)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
flip = function (x) {
  validImage(x)
  nd=as.list(rep(T, length(dim(x))))
  nd[[2]]=dim(x)[2]:1
  do.call('[', c(list(x),nd))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
flop = function (x) {
  validImage(x)
  nd=as.list(rep(T, length(dim(x))))
  nd[[1]]=dim(x)[1]:1
  do.call('[', c(list(x),nd))
}

## performs an affine transform on a set of images
affine <- function (x, m, filter=c("bilinear", "none"), output.dim) {
  ## check arguments
  validImage(x)
  if (!is.matrix(m) || nrow(m)!=3 || ncol(m)!=2) stop("'m' must be a 3x2 matrix")
  if (any(is.na(m))) stop("'m' shouldn't contain any NAs")
  m <- cbind(m, c(0, 0, 1))
  filter <- match.arg(filter)
  filter <- as.integer(c(none=0, bilinear=1)[filter])

  ## backtransform
  m <- solve(m)

  ## output image
  if (missing(output.dim)) {
    y <- Image(0, dim=dim(x), colormode=colorMode(x))
  } else {
    y <- Image(0, dim=c(output.dim[1], output.dim[2], tail(dim(x), -2)), colormode=colorMode(x))
  }
  
  return (.Call("affine", castImage(x), castImage(y), m, filter, PACKAGE='EBImage'))
}

rotate <- function(x, angle, filter="bilinear", output.origin=c(0, 0), output.dim) {
  theta <- angle*pi/180
  cx <- nrow(x)/2+nrow(x)*sqrt(2)*cos(theta-pi/4-pi/2)/2 + output.origin[1]
  cy <- ncol(x)/2+ncol(x)*sqrt(2)*sin(theta-pi/4-pi/2)/2 + output.origin[2]
  m <- matrix(c(cos(theta), -sin(theta), cx,
                sin(theta), cos(theta), cy), nrow=3)
  affine(x, m, filter, output.dim=output.dim)
}

translate <- function(x, v, filter="none", output.dim) {
  cx <- -v[1]
  cy <- -v[2]
  m <- matrix(c(1, 0, cx, 0, 1, cy), nrow=3)
  affine(x, m, filter=filter, output.dim=output.dim)
}

resize <- function(x, w, h, filter="bilinear", output.dim, output.origin=c(0, 0)) {
  ## checks
  if (missing(h)) h = round(w*dim(x)[2]/dim(x)[1])
  if (missing(output.dim)) output.dim <- c(w, h)
  else output.dim <- output.dim [1:2]
  
  ratio <- c(w, h)/dim(x)[1:2]
  m <-  matrix(c(ratio[1], 0, output.origin[1], 0, ratio[2], output.origin[2]), nrow=3)
  affine(x, m, filter, output.dim=output.dim)
}

## swaps the XY dimensions
## AO: note on performance: when a permutation of imageData rather than an Image object is needed as output it is much faster to issue the function directly on the Image object with 'keepClass = FALSE' than to extract the imageData first ond only then perform the swapping
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
swapXY = function(x, keepClass = TRUE) {
  validImage(x)
  dims = 1:length(dim(x))
  dims[1:2] = c(2:1)
  y = aperm(x, dims)
  if ( keepClass && is.Image(x) )
    x@.Data = y
  else 
    x = y
  x
}