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

  n = .numberOfFrames(x, 'total')
  if ( is.list(pt) ) pt = unlist(pt, use.names=FALSE)
  pt = as.integer(pt)
  if ( is.character(col) ) col = as.numeric(col2rgb(col)/255)
  col = as.numeric(matrix(col, nrow=n, ncol=1L))
  if ( any( pt<1L || pt>dim(x)[1:2] ) ) stop("coordinates 'pt' of the starting point(s) must be inside the image boundaries")

  return( .Call(C_floodFill, castImage(x), matrix(pt, nrow=n, ncol=2L, byrow=TRUE), col, as.numeric(tolerance)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fillHull = function(x) {
  validImage(x)
  return(.Call(C_fillHull, x))
}
