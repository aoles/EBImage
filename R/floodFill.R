# Flood fill for images and matrices

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
floodFill = function(x, pt, col, tolerance=0) {
  validImage(x)

  n = getNumberOfFrames(x, 'total')
  pt = matrix(as.integer(pt), nrow=n, ncol=2, byrow=TRUE)
  if (is.character(col)) col = as.numeric(col2rgb(col)/255)
  col = as.numeric(matrix(col, nrow=n, ncol=1))
  if (any(pt[,1]<1) || any(pt[,1]>dim(x)[1]) ||
      any(pt[,2]<1) || any(pt[,2]>dim(x)[2])) stop("coordinates 'pt' of the starting point(s) must be inside the image boundaries")

  return( .Call("floodFill", castImage(x), pt, col, as.numeric(tolerance), PACKAGE='EBImage'))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fillHull = function(x) {
  validImage(x)
  return(.Call("fillHull", castImage(x), PACKAGE='EBImage'))
}
