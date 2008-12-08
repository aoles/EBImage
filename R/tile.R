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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("tile", signature(x="list"),
    function (x, nx=10, lwd=1, fg.col="#E4AF2B", bg.col="black") {
        lapply(x, tile, nx=nx, lwd=lwd, fg.col=fg.col, bg.col=bg.col)
    }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("tile", signature(x="ImageX"),
  function (x, nx=10, lwd=1, fg.col="#E4AF2B", bg.col="gray") {
    if ( nx < 1 || lwd < 0 || lwd > 100 )
      stop( "wrong range of arguments, see help for range details" )
    hdr = Image(t(col2rgb(c(fg.col,bg.col)))/256,dim=c(2,1,3),col=Color)
    if (colorMode(x)==Grayscale) hdr=channel(hdr,'gray')
    else if (colorMode(x)==TrueColor) colorMode(hdr)=TrueColor
    x = .ImageCall("tile", x, hdr, as.integer(c(nx, lwd)) )
    return(x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("untile", signature(x="ImageX", nim="numeric"),
  function (x, nim, lwd=1) {
    nim = as.integer(nim)
    lwd = as.integer(lwd)
    if (length(nim)<2)
      stop("'nim' must contain two values for the number of images in x and y directions")
    if (lwd<0)
      stop("wrong line width")
    return(.ImageCall("untile", x, header(x), nim, lwd))
  }
)
