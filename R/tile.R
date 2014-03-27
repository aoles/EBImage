# Copyright (c) 2005-2007 Oleg Sklyar

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
tile = function (x, nx=10, lwd=1, fg.col="#E4AF2B", bg.col="gray") {
  if (is.list(x)) lapply(x, tile, nx=nx, lwd=lwd, fg.col=fg.col, bg.col=bg.col)
  else {
    validImage(x)
    if ( nx < 1 || lwd < 0 || lwd > 100 ) stop( "wrong range of arguments, see help for range details" )
    
    hdr = Image(c(fg.col,bg.col), dim = c(2,1), colormode = colorMode(x))
    
    .Call("tile", castImage(x), hdr, as.integer(c(nx, lwd)), PACKAGE='EBImage')
  }
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
untile = function (x, nim, lwd=1) {
  validImage(x)
  nim = as.integer(nim)
  lwd = as.integer(lwd)
  if (length(nim)<2) stop("'nim' must contain two values for the number of images in x and y directions")
  if (lwd<0) stop("wrong line width")
  .Call("untile", castImage(x), nim, lwd, PACKAGE='EBImage')
}
