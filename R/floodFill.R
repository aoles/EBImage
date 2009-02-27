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
floodFill = function(x, pt, col, tolerance=1e-3) {
  validImage(x)
  .dim = dim(x)
  pt = as.integer(pt)
  if (length(pt)<2)
    stop("'pt' must contain at least 2 values for x and y coordinates")
  if (any(pt)<1 || any(pt[1:2]>.dim[1:2]))
    stop("coordinates of the start point must be inside the image boundaries")
  tolerance = as.numeric(tolerance)
  if (missing(col)) col=x[pt[1]+pt[2]*.dim[1]]
  ## allows for conversion from X11 color string to its RGB or grayscale value
  if (is.character(col))
    col <- if (is.integer(x)) channel(col,"rgb") else channel(col,"gray")
  else
    col <- if (is.integer(x)) as.integer(col) else col=as.double(col)
  return( .Call("floodFill", castImage(x), pt, col, tolerance, PACKAGE='EBImage'))
} 

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fillHull = function(x) {
  validImage(x)
  return(.Call("fillHull", castImage(x), PACKAGE='EBImage'))
}
