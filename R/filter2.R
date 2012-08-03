# 2D convolution-based linear filter for images and matrix data

# Copyright (c) 2007 Gregoire Pau, Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filter2 = function(x, filter) {
  validObject(x)
  validObject(filter)

  dx = dim(x)
  cmx = colorMode(x)
  df = dim(filter)

  if (any(df%%2==0)) stop("dimensions of 'filter' matrix must be odd")
  if (any(dx[1:2]<df)) stop("dimensions of 'x' must be bigger than 'filter'")

  ## find centres of x and filter
  cx = dx%/%2
  cf = df%/%2

  ## create fft filter matrix
  wf = matrix(0.0, nrow=dx[1], ncol=dx[2])
  wf[(cx[1]-cf[1]):(cx[1]+cf[1]),(cx[2]-cf[2]):(cx[2]+cf[2])] = filter
  wf = fft(wf)

  ## convert to a frame-based 3D array
  dim(x) = c(dx[1:2],prod(dx)/prod(dx[1:2]))

  index1 = c(cx[1]:dx[1],1:(cx[1]-1))
  index2 = c(cx[2]:dx[2],1:(cx[2]-1))
  pdx = prod(dim(x)[1:2])
  y = apply(x, 3, function(xx) {
    dim(xx) = dx[1:2]
    Re(fft(fft(xx)*wf, inverse=T)/pdx)[index1,index2]
  })
  dim(y) = dx

  ## convert it back
  if (is.Image(x)) return(Image(y, colormode=cmx))
  else return(y)
}



