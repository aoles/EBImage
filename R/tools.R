# Package tools

# Copyright (c) 2006 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

## checks whether 'x' is a suitable image
validImage=function(x) {
  z = validImageObject(x)
  if (isTRUE(z))
    TRUE
  else 
    stop(z)
}

## if required changes the storage.mode of 'x' to 'double' 
castImage = function(x) {
  if (!is.double(x)) storage.mode(x) = 'double'
  x
}

## clip pixel data to the specified range
clipImage = function(x, range = c(0, 1)) {
  normalize(x, ft = NULL, inputRange = range)
}

## check if x (indexing image) and ref (image) are compatible
checkCompatibleImages=function(x, ref, type='total') {
  xn = deparse(substitute(x), width.cutoff = 500L, nlines = 1)
  refn = deparse(substitute(ref), width.cutoff = 500L, nlines = 1)
  validImage(x)
  if (!missing(ref)) {
    validImage(ref)
    if (getNumberOfFrames(x, type)!=getNumberOfFrames(ref, type)) stop( "'", xn, "' and '", refn, "' must have the same ",type," number of frames" )
    if (any(dim(x)[1:2]!=dim(ref)[1:2])  ) stop( "'", xn, "' and '", refn, "' must have the same spatial 2D dimensions" )
  }
}
