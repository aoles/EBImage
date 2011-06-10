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
  nd[[1]]=dim(x)[2]:1
  do.call('[', c(list(x),nd))
}

## Translate a set of images according to a matrix of translation
## This C function is needed for performance reasons !
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
translate = function (x, v) {
  validImage(x)
  v = matrix(v, nrow=getNumberOfFrames(x,'total'), ncol=2, byrow=TRUE)
  if (length(v)!=2*getNumberOfFrames(x,'total')) stop("'v' must be a matrix of size (n,2), where \'n'\ is the total number of frames")
  if (any(is.na(v))) stop("'v' shouldn't contain any NAs")
  
  return (.Call("translate", castImage(x), v, PACKAGE='EBImage'))
}

## Do an affine transform on a set of images
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
affine = function (x, m) {
  validImage(x)
  if (!is.matrix(m) || nrow(m)!=3 || ncol(m)!=2) stop("'m' must be a 3x2 matrix")
  if (any(is.na(m))) stop("'m' shouldn't contain any NAs")
  m = cbind(m, c(0, 0, 1))
  ## backtransform
  m = solve(m)
  return (.Call("affine", castImage(x), m, PACKAGE='EBImage'))
}
